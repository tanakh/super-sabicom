use educe::Educe;
use log::trace;
use meru_interface::{AudioBuffer, AudioSample};
use modular_bitfield::prelude::*;

#[derive(Educe)]
#[educe(Default)]
pub struct Dsp {
    master_volume: [i8; 2],
    echo_volume: [i8; 2],
    flags: Flags,
    echo_feedback: i8,
    sample_table_addr: u8,
    echo_buf_addr: u8,
    echo_buf_size: u8,
    na: u8,

    voice: [Voice; 8],
    #[educe(Default = 1)]
    noise: i16,
    noise_counter: u16,

    #[educe(Default(expression = "vec![0; 0x10000]"))]
    pub ram: Vec<u8>,

    pub audio_buffer: AudioBuffer,
}

#[bitfield(bits = 8)]
struct Flags {
    // 0-4  Noise frequency    (0=Stop, 1=16Hz, 2=21Hz, ..., 1Eh=16kHz, 1Fh=32kHz)
    noise_freq: B5,
    disable_echo_buf_write: bool,
    mute: bool,
    reset: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Self::new()
            .with_disable_echo_buf_write(true)
            .with_mute(true)
            .with_reset(true)
    }
}

#[derive(Default)]
struct Voice {
    key_on: bool,
    key_off: bool,
    enable_pitch_modulation: bool,
    enable_noise: bool,
    enable_echo: bool,
    voice_end: bool,

    volume: [i8; 2],
    fir_coeff: i8,
    sample_rate: u16, // 14 bit sample rate (bit 14,15 are read/write-able)
    source_num: u8,
    adsr_setting: AdsrSetting,
    gain_setting: u8,
    na: [u8; 3],

    cur_envelope: u16, // current value of 11bit envelope
    cur_sample: i16,   // current value of 15bit sample

    state: EnvelopeState,
    brr_cur_addr: u16,
    brr_loop_addr: u16,
    brr_pitch_counter: u16,
    brr_cur_header: BrrBlockHeader,
    brr_buf: [i16; 16],
    brr_old: [i16; 2],
    gauss_old: [i16; 4],
    envelope_counter: u16,
}

#[derive(Default)]
enum EnvelopeState {
    #[default]
    Attack,
    Decay,
    Sustain,
    Release,
}

#[bitfield(bits = 8)]
#[derive(Default, Debug)]
struct BrrBlockHeader {
    end: bool,
    repeat: bool,
    filter_num: B2,
    shift: B4,
}

#[bitfield(bits = 16)]
#[derive(Default)]
struct AdsrSetting {
    attack_rate: B4, // Rate=N*2+1, Step=+32 (or Step=+1024 when Rate=31)
    decay_rate: B3,  // Rate=N*2+16, Step=-(((Level-1) SAR 8)+1)
    use_adsr: bool,
    sustain_rate: B5,  // Rate=N, Step=-(((Level-1) SAR 8)+1)
    sustain_level: B3, // Boundary=(N+1)*100h
}

const RATE_TABLE: [u16; 32] = [
    0, 2048, 1536, 1280, 1024, 768, 640, 512, 384, 320, 256, 192, 160, 128, 96, 80, 64, 48, 40, 32,
    24, 20, 16, 12, 10, 8, 6, 5, 4, 3, 2, 1,
];

impl Voice {
    fn tick(&mut self, ram: &[u8], sample_table_addr: u8, prev_out: Option<i16>, noise: i16) {
        if self.key_on {
            self.key_on = false;

            self.cur_envelope = 0;
            self.envelope_counter = 0;
            self.state = EnvelopeState::Attack;

            let brr_addr = sample_table_addr as u16 * 0x100 + self.source_num as u16 * 4;
            self.brr_start(ram, brr_addr);
        }

        if self.key_off {
            self.key_off = false;
            self.state = EnvelopeState::Release;
        }

        let mut step = self.sample_rate & 0x3FFF;

        if self.enable_pitch_modulation && prev_out.is_some() {
            let factor = (prev_out.unwrap() >> 4) + 0x400;
            step = ((step as i32 * factor as i32) >> 10) as u16;
            step = step.min(0x3FFF); // ???
        }

        let mut prev_brr_ix = ((self.brr_pitch_counter >> 12) & 0xF) as usize;
        let (counter, ovf) = self.brr_pitch_counter.overflowing_add(step);

        if ovf {
            for i in prev_brr_ix + 1..16 {
                self.push_hist(self.brr_buf[i]);
            }
            self.brr_next(ram);
            prev_brr_ix = 0;
        }

        let cur_brr_ix = ((counter >> 12) & 0xF) as usize;
        for i in prev_brr_ix + 1..=cur_brr_ix {
            self.push_hist(self.brr_buf[i]);
        }

        self.brr_pitch_counter = counter;

        // BRR decode and check end flag are performed even if noise is enabled
        let sample = if !self.enable_noise {
            let interpol_ix = ((counter >> 4) & 0xFF) as usize;
            self.gaussian_interpolation(interpol_ix)
        } else {
            noise
        };

        // Calculate envelope

        self.cur_envelope &= 0x7FF;

        if !matches!(self.state, EnvelopeState::Release) && !self.adsr_setting.use_adsr() {
            if self.gain_setting & 0x80 == 0 {
                self.cur_envelope = (self.gain_setting & 0x7F) as u16 * 16;
            } else {
                let rate = self.gain_setting & 0x1F;
                let mode = (self.gain_setting >> 5) & 3;

                self.envelope_counter += 1;

                if rate == 0 {
                    self.envelope_counter = 0;
                } else if self.envelope_counter >= RATE_TABLE[rate as usize] {
                    self.envelope_counter = 0;

                    match mode {
                        0 => self.cur_envelope = self.cur_envelope.saturating_sub(32),
                        1 => {
                            if self.cur_envelope > 0 {
                                let step = (self.cur_envelope - 1) >> 8 + 1;
                                self.cur_envelope = self.cur_envelope.saturating_sub(step);
                            }
                        }
                        2 => self.cur_envelope = (self.cur_envelope + 32).min(0x7FF),
                        3 => {
                            if self.cur_envelope < 0x600 {
                                self.cur_envelope += 32;
                            } else {
                                self.cur_envelope = (self.cur_envelope + 8).min(0x7FF)
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
        } else {
            match self.state {
                EnvelopeState::Attack => {
                    let rate = self.adsr_setting.attack_rate() * 2 + 1;
                    self.envelope_counter += 1;
                    if self.envelope_counter >= RATE_TABLE[rate as usize] {
                        self.envelope_counter = 0;
                        self.cur_envelope =
                            (self.cur_envelope + if rate != 31 { 32 } else { 1024 }).min(0x7FF);
                        if self.cur_envelope >= 0x7E0 {
                            self.state = EnvelopeState::Decay;
                        }
                    }
                }
                EnvelopeState::Decay => {
                    let rate = self.adsr_setting.decay_rate() * 2 + 16;
                    self.envelope_counter += 1;
                    if self.envelope_counter >= RATE_TABLE[rate as usize] {
                        self.envelope_counter = 0;
                        let step = ((self.cur_envelope - 1) >> 8) + 1;
                        self.cur_envelope -= step;
                        let boundary = (self.adsr_setting.sustain_level() as u16 + 1) * 0x100;
                        if self.cur_envelope <= boundary {
                            self.state = EnvelopeState::Sustain;
                        }
                    }
                }
                EnvelopeState::Sustain => {
                    let rate = self.adsr_setting.sustain_rate();
                    if rate == 0 {
                        self.envelope_counter = 0;
                    } else {
                        self.envelope_counter += 1;
                        if self.envelope_counter >= RATE_TABLE[rate as usize] {
                            self.envelope_counter = 0;
                            if self.cur_envelope > 0 {
                                let step = ((self.cur_envelope - 1) >> 8) + 1;
                                self.cur_envelope -= step;
                            }
                        }
                    }
                }
                EnvelopeState::Release => {
                    self.envelope_counter += 1;
                    if self.envelope_counter >= RATE_TABLE[31] {
                        self.envelope_counter = 0;
                        self.cur_envelope = self.cur_envelope.saturating_sub(8);
                    }
                }
            }
        }

        self.cur_sample = ((sample as i32 * self.cur_envelope as i32) >> 11) as i16;
    }

    fn brr_start(&mut self, ram: &[u8], addr: u16) {
        let addr = addr as usize;
        self.brr_cur_addr = u16::from_le_bytes(ram[addr..addr + 2].try_into().unwrap());
        self.brr_loop_addr = u16::from_le_bytes(ram[addr + 2..addr + 4].try_into().unwrap());
        self.brr_pitch_counter = 0;
        self.brr_decode_block(ram);
    }

    fn brr_next(&mut self, ram: &[u8]) {
        if !self.brr_cur_header.end() {
            self.brr_decode_block(ram);
        } else if self.brr_cur_header.repeat() {
            self.voice_end = true;
            self.brr_cur_addr = self.brr_loop_addr;
            // log::debug!("BRR repeat: addr = {:04X}", self.brr_cur_addr);
            self.brr_decode_block(ram);
        } else {
            self.voice_end = true;
            self.brr_cur_addr = self.brr_loop_addr;
            self.state = EnvelopeState::Release;
            self.cur_envelope = 0;
            // log::debug!("BRR end: addr = {:04X}", self.brr_cur_addr);
            self.brr_decode_block(ram);
        }
    }

    fn brr_decode_block(&mut self, ram: &[u8]) {
        let block = &ram[self.brr_cur_addr as usize..self.brr_cur_addr as usize + 9];
        self.brr_cur_addr = self.brr_cur_addr.wrapping_add(9);

        let header = BrrBlockHeader::from_bytes([block[0]]);

        for i in 0..16 {
            let nibble = block[1 + i / 2] >> ((i & 1 ^ 1) * 4);
            let nibble = ((nibble as i16) << 12) >> 12;
            let shift = header.shift();
            let sample = if shift <= 12 {
                (nibble << shift) >> 1
            } else {
                // When shift=13..15, decoding works as if shift=12 and nibble=(nibble SAR 3).
                ((nibble >> 3) << 12) >> 1
            };

            // FIXME: Filter overflow behavior
            // glitches will occur:
            // If new>+7FFFh then new=+7FFFh (but, clipped to +3FFFh below) ;\clamp 16bit
            // If new<-8000h then new=-8000h (but, clipped to ZERO below)   ;/(dirt-effect)
            // If new=(+4000h..+7FFFh) then new=(-4000h..-1)                ;\clip 15bit
            // If new=(-8000h..-4001h) then new=(-0..-3FFFh)                ;/(lost-sign)
            // If new>+3FF8h OR new<-3FFAh then overflows can occur in Gauss section

            let old = self.brr_old[0] as i32;
            let older = self.brr_old[1] as i32;
            let sample = sample as i32;

            let new = match header.filter_num() {
                0 => sample,
                1 => sample + old + ((-old) >> 4),
                2 => sample + old * 2 + ((-old * 3) >> 5) - older + (older >> 4),
                3 => sample + old * 2 + ((-old * 13) >> 6) - older + ((older * 3) >> 4),
                _ => unreachable!(),
            };

            // 15bit signed value
            let new = ((new << 1) >> 1) as i16;

            self.brr_buf[i] = new;
            self.brr_old[1] = self.brr_old[0];
            self.brr_old[0] = new;
        }

        self.brr_cur_header = header;
        self.push_hist(self.brr_buf[0]);
    }

    fn push_hist(&mut self, sample: i16) {
        self.gauss_old[3] = self.gauss_old[2];
        self.gauss_old[2] = self.gauss_old[1];
        self.gauss_old[1] = self.gauss_old[0];
        self.gauss_old[0] = sample;
    }

    fn gaussian_interpolation(&self, ix: usize) -> i16 {
        let f3 = ((self.gauss_old[3] as i32 * GAUSS_TABLE[0xFF - ix] as i32) >> 10) as i16;
        let f2 = ((self.gauss_old[2] as i32 * GAUSS_TABLE[0x1FF - ix] as i32) >> 10) as i16;
        let f1 = ((self.gauss_old[1] as i32 * GAUSS_TABLE[0x100 + ix] as i32) >> 10) as i16;
        let f0 = ((self.gauss_old[0] as i32 * GAUSS_TABLE[ix] as i32) >> 10) as i16;

        let out = f3.wrapping_add(f2);
        let out = out.wrapping_add(f1);
        let out = out.saturating_add(f0);

        out >> 1
    }
}

impl Dsp {
    pub fn tick(&mut self) {
        self.tick_noise();

        for ch in 0..8 {
            let prev_out = if ch == 0 {
                None
            } else {
                Some(self.voice[ch - 1].cur_sample)
            };
            self.voice[ch].tick(&self.ram, self.sample_table_addr, prev_out, self.noise);
        }

        let mut output = [0; 2];

        for i in 0..2 {
            let mut sum = 0;

            for ch in 0..8 {
                let sample = ((self.voice[ch].cur_sample << 1) as i16 as i32) >> 1;
                let c = (sample * self.voice[ch].volume[i] as i32) >> 6;
                sum = (sum + c).clamp(-0x8000, 0x7FFF);
            }

            sum = (sum * self.master_volume[i] as i32) >> 7;

            // TODO: sum = sum + (fir_out*EVOLx SAR 7)

            if self.flags.mute() {
                sum = 0;
            }

            // FIXME: ???
            // sum = sum XOR FFFFh  ;-final phase inversion (as done by built-in post-amp)

            output[i] = sum as i16;
        }

        let output = AudioSample {
            left: output[0],
            right: output[1],
        };

        self.audio_buffer.samples.push(output);
    }

    fn tick_noise(&mut self) {
        let rate = self.flags.noise_freq();
        if rate == 0 {
            self.noise_counter = 0;
            return;
        }

        self.noise_counter += 1;
        if self.noise_counter >= RATE_TABLE[rate as usize] {
            self.noise_counter = 0;
            let b = (self.noise ^ (self.noise >> 1)) & 1;
            self.noise = ((self.noise & 0x7FFF) | (b << 15)) >> 1;
        }
    }

    pub fn read(&mut self, addr: u8) -> u8 {
        let data = match addr & 0x7F {
            //   0Ch - MVOLL    - Left channel master volume (R/W)
            0x0C => self.master_volume[0] as u8,
            //   1Ch - MVOLR    - Right channel master volume (R/W)
            0x1C => self.master_volume[1] as u8,
            //   2Ch - EVOLL    - Left channel echo volume (R/W)
            0x2C => self.echo_volume[0] as u8,
            //   3Ch - EVOLR    - Right channel echo volume (R/W)
            0x3C => self.echo_volume[1] as u8,
            //   4Ch - KON      - Key On Flags for Voice 0..7 (W)
            0x4C => {
                // FIXME: Is this correct?
                let mut ret = 0;
                for ch in 0..8 {
                    ret |= (self.voice[ch].key_on as u8) << ch;
                }
                ret
            }
            //   5Ch - KOFF     - Key Off Flags for Voice 0..7 (R/W)
            0x5C => {
                // FIXME: Is this correct?
                let mut ret = 0;
                for ch in 0..8 {
                    ret |= (self.voice[ch].key_off as u8) << ch;
                }
                ret
            }
            //   6Ch - FLG      - Reset, Mute, Echo-Write flags and Noise Clock (R/W)
            0x6C => self.flags.bytes[0],
            //   7Ch - ENDX     - Voice End Flags for Voice 0..7 (R) (W=Ack)
            0x7C => {
                let mut ret = 0;
                for ch in 0..8 {
                    ret |= (self.voice[ch].voice_end as u8) << ch;
                }
                ret
            }
            //   0Dh - EFB      - Echo feedback volume (R/W)
            0x0D => self.echo_feedback as u8,
            //   1Dh - NA       - Unused (1 byte of general-purpose RAM) (R/W)
            0x1D => self.na,
            //   2Dh - PMON     - Pitch Modulation Enable Flags for Voice 1..7 (R/W)
            0x2D => {
                let mut ret = 0;
                for ch in 1..8 {
                    ret |= (self.voice[ch].enable_pitch_modulation as u8) << ch;
                }
                ret
            }
            //   3Dh - NON      - Noise Enable Flags for Voice 0..7 (R/W)
            0x3D => {
                let mut ret = 0;
                for ch in 0..8 {
                    ret |= (self.voice[ch].enable_noise as u8) << ch;
                }
                ret
            }
            //   4Dh - EON      - Echo Enable Flags for Voice 0..7 (R/W)
            0x4D => {
                let mut ret = 0;
                for ch in 0..8 {
                    ret |= (self.voice[ch].enable_echo as u8) << ch;
                }
                ret
            }
            //   5Dh - DIR      - Sample table address (R/W)
            0x5D => self.sample_table_addr,
            //   6Dh - ESA      - Echo ring buffer address (R/W)
            0x6D => self.echo_buf_addr,
            //   7Dh - EDL      - Echo delay (ring buffer size) (R/W)
            0x7D => self.echo_buf_size,

            _ => {
                let ch = ((addr >> 4) & 7) as usize;
                self.voice[ch].read(addr & 0xF)
            }
        };

        trace!("DSP Register Read : {addr:02X} -> {data:02X}");

        data
    }

    pub fn write(&mut self, addr: u8, data: u8) {
        trace!("DSP Register Write: {addr:02X} <- {data:02X}");

        match addr & 0x7F {
            //   0Ch - MVOLL    - Left channel master volume (R/W)
            0x0C => self.master_volume[0] = data as i8,
            //   1Ch - MVOLR    - Right channel master volume (R/W)
            0x1C => self.master_volume[1] = data as i8,
            //   2Ch - EVOLL    - Left channel echo volume (R/W)
            0x2C => self.echo_volume[0] = data as i8,
            //   3Ch - EVOLR    - Right channel echo volume (R/W)
            0x3C => self.echo_volume[1] = data as i8,
            //   4Ch - KON      - Key On Flags for Voice 0..7 (W)
            0x4C => {
                for ch in 0..8 {
                    self.voice[ch].key_on = data & (1 << ch) != 0;
                }
            }

            //   5Ch - KOFF     - Key Off Flags for Voice 0..7 (R/W)
            0x5C => {
                for ch in 0..8 {
                    self.voice[ch].key_off = data & (1 << ch) != 0;
                }
            }
            //   6Ch - FLG      - Reset, Mute, Echo-Write flags and Noise Clock (R/W)
            0x6C => {
                self.flags.bytes[0] = data;
                if self.flags.reset() {
                    for ch in 0..8 {
                        self.voice[ch].key_off = true;
                        self.voice[ch].cur_envelope = 0;
                    }

                    // FIXME: This bit is cleared when read?
                    self.flags.set_reset(false);
                }
            }
            //   7Ch - ENDX     - Voice End Flags for Voice 0..7 (R) (W=Ack)
            0x7C => {
                for ch in 0..8 {
                    self.voice[ch].voice_end = false;
                }
            }
            //   0Dh - EFB      - Echo feedback volume (R/W)
            0x0D => self.echo_feedback = data as i8,
            //   1Dh - NA       - Unused (1 byte of general-purpose RAM) (R/W)
            0x1D => self.na = data,
            //   2Dh - PMON     - Pitch Modulation Enable Flags for Voice 1..7 (R/W)
            0x2D => {
                for ch in 0..8 {
                    self.voice[ch].enable_pitch_modulation = data & (1 << ch) != 0;
                }
            }
            //   3Dh - NON      - Noise Enable Flags for Voice 0..7 (R/W)
            0x3D => {
                for ch in 0..8 {
                    self.voice[ch].enable_noise = data & (1 << ch) != 0;
                }
            }
            //   4Dh - EON      - Echo Enable Flags for Voice 0..7 (R/W)
            0x4D => {
                for ch in 0..8 {
                    self.voice[ch].enable_echo = data & (1 << ch) != 0;
                }
            }
            //   5Dh - DIR      - Sample table address (R/W)
            0x5D => self.sample_table_addr = data,
            //   6Dh - ESA      - Echo ring buffer address (R/W)
            0x6D => self.echo_buf_addr = data,
            //   7Dh - EDL      - Echo delay (ring buffer size) (R/W)
            0x7D => self.echo_buf_size = data,

            _ => {
                let ch = ((addr >> 4) & 7) as usize;
                self.voice[ch].write(addr & 0xF, data);
            }
        }
    }
}

impl Voice {
    fn read(&mut self, addr: u8) -> u8 {
        match addr {
            // x0h - VxVOLL   - Left volume for Voice 0..7 (R/W)
            0x0 => self.volume[0] as u8,
            // x1h - VxVOLR   - Right volume for Voice 0..7 (R/W)
            0x1 => self.volume[1] as u8,
            // x2h - VxPITCHL - Pitch scaler for Voice 0..7, lower 8bit (R/W)
            0x2 => self.sample_rate as u8,
            // x3h - VxPITCHH - Pitch scaler for Voice 0..7, upper 6bit (R/W)
            0x3 => (self.sample_rate >> 8) as u8,
            // x4h - VxSRCN   - Source number for Voice 0..7 (R/W)
            0x4 => self.source_num,
            // x5h - VxADSR1  - ADSR settings for Voice 0..7, lower 8bit (R/W)
            0x5 => self.adsr_setting.bytes[0],
            // x6h - VxADSR2  - ADSR settings for Voice 0..7, upper 8bit (R/W)
            0x6 => self.adsr_setting.bytes[1],
            // x7h - VxGAIN   - Gain settings for Voice 0..7 (R/W)
            0x7 => self.gain_setting,
            // x8h - VxENVX   - Current envelope value for Voice 0..7 (R)
            0x8 => (self.cur_envelope >> 4) as u8,
            // x9h - VxOUTX   - Current sample value for Voice 0..7 (R)
            0x9 => (self.cur_sample >> 7) as u8,
            // xAh - NA       - Unused (8 bytes of general-purpose RAM) (R/W)
            0xA => self.na[0],
            // xBh - NA       - Unused (8 bytes of general-purpose RAM) (R/W)
            0xB => self.na[1],
            // xEh - NA       - Unused (8 bytes of general-purpose RAM) (R/W)
            0xE => self.na[2],
            // xFh - FIRx     - Echo FIR filter coefficient 0..7 (R/W)
            0xF => self.fir_coeff as u8,
            _ => unreachable!(),
        }
    }

    fn write(&mut self, addr: u8, data: u8) {
        match addr {
            // x0h - VxVOLL   - Left volume for Voice 0..7 (R/W)
            0x0 => self.volume[0] = data as i8,
            // x1h - VxVOLR   - Right volume for Voice 0..7 (R/W)
            0x1 => self.volume[1] = data as i8,
            // x2h - VxPITCHL - Pitch scaler for Voice 0..7, lower 8bit (R/W)
            0x2 => self.sample_rate = self.sample_rate & 0xFF00 | data as u16,
            // x3h - VxPITCHH - Pitch scaler for Voice 0..7, upper 6bit (R/W)
            0x3 => self.sample_rate = self.sample_rate & 0xFF | (data as u16) << 8,
            // x4h - VxSRCN   - Source number for Voice 0..7 (R/W)
            0x4 => self.source_num = data,
            // x5h - VxADSR1  - ADSR settings for Voice 0..7, lower 8bit (R/W)
            0x5 => self.adsr_setting.bytes[0] = data,
            // x6h - VxADSR2  - ADSR settings for Voice 0..7, upper 8bit (R/W)
            0x6 => self.adsr_setting.bytes[1] = data,
            // x7h - VxGAIN   - Gain settings for Voice 0..7 (R/W)
            0x7 => self.gain_setting = data,
            // x8h - VxENVX   - Current envelope value for Voice 0..7 (R)
            0x8 => self.cur_envelope = (data as u16) << 4,
            // x9h - VxOUTX   - Current sample value for Voice 0..7 (R)
            0x9 => self.cur_sample = ((data as u16) << 7) as i16,
            // xAh - NA       - Unused (8 bytes of general-purpose RAM) (R/W)
            0xA => self.na[0] = data,
            // xBh - NA       - Unused (8 bytes of general-purpose RAM) (R/W)
            0xB => self.na[1] = data,
            // xEh - NA       - Unused (8 bytes of general-purpose RAM) (R/W)
            0xE => self.na[2] = data,
            // xFh - FIRx     - Echo FIR filter coefficient 0..7 (R/W)
            0xF => self.fir_coeff = data as i8,
            _ => unreachable!(),
        }
    }
}

#[rustfmt::skip]
const GAUSS_TABLE: [u16; 512] = [
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
    0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x002, 0x002, 0x002, 0x002, 0x002,
    0x002, 0x002, 0x003, 0x003, 0x003, 0x003, 0x003, 0x004, 0x004, 0x004, 0x004, 0x004, 0x005, 0x005, 0x005, 0x005,
    0x006, 0x006, 0x006, 0x006, 0x007, 0x007, 0x007, 0x008, 0x008, 0x008, 0x009, 0x009, 0x009, 0x00A, 0x00A, 0x00A,
    0x00B, 0x00B, 0x00B, 0x00C, 0x00C, 0x00D, 0x00D, 0x00E, 0x00E, 0x00F, 0x00F, 0x00F, 0x010, 0x010, 0x011, 0x011,
    0x012, 0x013, 0x013, 0x014, 0x014, 0x015, 0x015, 0x016, 0x017, 0x017, 0x018, 0x018, 0x019, 0x01A, 0x01B, 0x01B,
    0x01C, 0x01D, 0x01D, 0x01E, 0x01F, 0x020, 0x020, 0x021, 0x022, 0x023, 0x024, 0x024, 0x025, 0x026, 0x027, 0x028,
    0x029, 0x02A, 0x02B, 0x02C, 0x02D, 0x02E, 0x02F, 0x030, 0x031, 0x032, 0x033, 0x034, 0x035, 0x036, 0x037, 0x038,
    0x03A, 0x03B, 0x03C, 0x03D, 0x03E, 0x040, 0x041, 0x042, 0x043, 0x045, 0x046, 0x047, 0x049, 0x04A, 0x04C, 0x04D,
    0x04E, 0x050, 0x051, 0x053, 0x054, 0x056, 0x057, 0x059, 0x05A, 0x05C, 0x05E, 0x05F, 0x061, 0x063, 0x064, 0x066,
    0x068, 0x06A, 0x06B, 0x06D, 0x06F, 0x071, 0x073, 0x075, 0x076, 0x078, 0x07A, 0x07C, 0x07E, 0x080, 0x082, 0x084,
    0x086, 0x089, 0x08B, 0x08D, 0x08F, 0x091, 0x093, 0x096, 0x098, 0x09A, 0x09C, 0x09F, 0x0A1, 0x0A3, 0x0A6, 0x0A8,
    0x0AB, 0x0AD, 0x0AF, 0x0B2, 0x0B4, 0x0B7, 0x0BA, 0x0BC, 0x0BF, 0x0C1, 0x0C4, 0x0C7, 0x0C9, 0x0CC, 0x0CF, 0x0D2,
    0x0D4, 0x0D7, 0x0DA, 0x0DD, 0x0E0, 0x0E3, 0x0E6, 0x0E9, 0x0EC, 0x0EF, 0x0F2, 0x0F5, 0x0F8, 0x0FB, 0x0FE, 0x101,
    0x104, 0x107, 0x10B, 0x10E, 0x111, 0x114, 0x118, 0x11B, 0x11E, 0x122, 0x125, 0x129, 0x12C, 0x130, 0x133, 0x137,
    0x13A, 0x13E, 0x141, 0x145, 0x148, 0x14C, 0x150, 0x153, 0x157, 0x15B, 0x15F, 0x162, 0x166, 0x16A, 0x16E, 0x172,
    0x176, 0x17A, 0x17D, 0x181, 0x185, 0x189, 0x18D, 0x191, 0x195, 0x19A, 0x19E, 0x1A2, 0x1A6, 0x1AA, 0x1AE, 0x1B2,
    0x1B7, 0x1BB, 0x1BF, 0x1C3, 0x1C8, 0x1CC, 0x1D0, 0x1D5, 0x1D9, 0x1DD, 0x1E2, 0x1E6, 0x1EB, 0x1EF, 0x1F3, 0x1F8,
    0x1FC, 0x201, 0x205, 0x20A, 0x20F, 0x213, 0x218, 0x21C, 0x221, 0x226, 0x22A, 0x22F, 0x233, 0x238, 0x23D, 0x241,
    0x246, 0x24B, 0x250, 0x254, 0x259, 0x25E, 0x263, 0x267, 0x26C, 0x271, 0x276, 0x27B, 0x280, 0x284, 0x289, 0x28E,
    0x293, 0x298, 0x29D, 0x2A2, 0x2A6, 0x2AB, 0x2B0, 0x2B5, 0x2BA, 0x2BF, 0x2C4, 0x2C9, 0x2CE, 0x2D3, 0x2D8, 0x2DC,
    0x2E1, 0x2E6, 0x2EB, 0x2F0, 0x2F5, 0x2FA, 0x2FF, 0x304, 0x309, 0x30E, 0x313, 0x318, 0x31D, 0x322, 0x326, 0x32B,
    0x330, 0x335, 0x33A, 0x33F, 0x344, 0x349, 0x34E, 0x353, 0x357, 0x35C, 0x361, 0x366, 0x36B, 0x370, 0x374, 0x379,
    0x37E, 0x383, 0x388, 0x38C, 0x391, 0x396, 0x39B, 0x39F, 0x3A4, 0x3A9, 0x3AD, 0x3B2, 0x3B7, 0x3BB, 0x3C0, 0x3C5,
    0x3C9, 0x3CE, 0x3D2, 0x3D7, 0x3DC, 0x3E0, 0x3E5, 0x3E9, 0x3ED, 0x3F2, 0x3F6, 0x3FB, 0x3FF, 0x403, 0x408, 0x40C,
    0x410, 0x415, 0x419, 0x41D, 0x421, 0x425, 0x42A, 0x42E, 0x432, 0x436, 0x43A, 0x43E, 0x442, 0x446, 0x44A, 0x44E,
    0x452, 0x455, 0x459, 0x45D, 0x461, 0x465, 0x468, 0x46C, 0x470, 0x473, 0x477, 0x47A, 0x47E, 0x481, 0x485, 0x488,
    0x48C, 0x48F, 0x492, 0x496, 0x499, 0x49C, 0x49F, 0x4A2, 0x4A6, 0x4A9, 0x4AC, 0x4AF, 0x4B2, 0x4B5, 0x4B7, 0x4BA,
    0x4BD, 0x4C0, 0x4C3, 0x4C5, 0x4C8, 0x4CB, 0x4CD, 0x4D0, 0x4D2, 0x4D5, 0x4D7, 0x4D9, 0x4DC, 0x4DE, 0x4E0, 0x4E3,
    0x4E5, 0x4E7, 0x4E9, 0x4EB, 0x4ED, 0x4EF, 0x4F1, 0x4F3, 0x4F5, 0x4F6, 0x4F8, 0x4FA, 0x4FB, 0x4FD, 0x4FF, 0x500,
    0x502, 0x503, 0x504, 0x506, 0x507, 0x508, 0x50A, 0x50B, 0x50C, 0x50D, 0x50E, 0x50F, 0x510, 0x511, 0x511, 0x512,
    0x513, 0x514, 0x514, 0x515, 0x516, 0x516, 0x517, 0x517, 0x517, 0x518, 0x518, 0x518, 0x518, 0x518, 0x519, 0x519,
];
