# FPGA-Oscilloscope-Project

- [ ] Stand-alone operation (no support from Matlab or other applications running on the host computer). 
- [x] VGA display (other options possible such as direct connect flat panel display) 
- [x] Vertical Position- Eli 
- [ ] Vertical Gain- Nathan 
- [ ] Horizontal Position- Eli 
- [ ] Horizontal Gain- Nathan 
- [ ] Trigger level- Eli
- [x] Triple Buffering
- [ ] Fix Rotary encoder
- [ ] Get ADC running at 1 MSample/s and don't fail timing (get rid of signals that cross clock domains)
- [ ] Probably should make it so that we write to ram blocks once a frame instead of continuously, otherwise non-periodic signals won't work (we have allthe information we need for one screen in one ram block)
