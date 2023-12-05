library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library UNISIM; 
use UNISIM.vcomponents.all;  --hi Eli
  
entity Scope_Project is
	port(
		clk:   in    std_logic;
		tx:    out   std_logic;
		rx:      in  std_logic;
		red:   out   std_logic_vector(1 downto 0);
		green: out   std_logic_vector(1 downto 0);
		blue:  out   std_logic_vector(1 downto 0);
		hsync: out   std_logic;
		vsync: out   std_logic;
		vaux5_n: in  std_logic;
		vaux5_p: in  std_logic;
		btn:      in std_logic_vector(1 downto 0);
		led:       out std_logic_vector(3 downto 0);
		pio31:   out std_logic;
		v_enc_d: in std_logic;  --pin 47 encoder DT 
		v_enc_clk: in std_logic;   --pin 48 encoder CLK
		enc_btn: in std_logic   --pin 46 encoder CLK
	);
end Scope_Project;

architecture arch of Scope_Project is
	component lab05_adc is
		port(
			clk_i:    in  std_logic;
			vaux5n_i: in  std_logic;
			vaux5p_i: in  std_logic;
			rdy_o:    out std_logic;
			data_o:   out std_logic_vector(11 downto 0)
		);
	end component;
	component lab05_ram is
		port(
			clka_i:  in  std_logic;
			wea_i:   in  std_logic;
			addra_i: in  std_logic_vector(9 downto 0);
			dataa_i: in  std_logic_vector(35 downto 0);
			dataa_o: out std_logic_vector(35 downto 0);
			clkb_i:  in  std_logic;
			web_i:   in  std_logic;
			addrb_i: in  std_logic_vector(9 downto 0);
			datab_i: in  std_logic_vector(35 downto 0);
			datab_o: out std_logic_vector(35 downto 0)
		);
	end component;
	component lab05_cmt is
		port(
			clk_i: in  std_logic;
			clk_o: out std_logic
		);
	end component;
	
	constant samples: natural:=639;
	constant timeout_thresh: natural:=208000000;
	signal fclk:  std_logic;
	signal rdy:   std_logic;
	signal thrsh: std_logic_vector(11 downto 0); --set to 3000 to before we want to adjust it
	signal addra: std_logic_vector(9 downto 0);
	signal addra0: std_logic_vector(9 downto 0);
	signal addra1: std_logic_vector(9 downto 0);
	signal addra2: std_logic_vector(9 downto 0);
	signal dataa: std_logic_vector(35 downto 0);
    signal dataa0: std_logic_vector(35 downto 0);
    signal dataa1: std_logic_vector(35 downto 0);
    signal dataa2: std_logic_vector(35 downto 0);
	signal addrb: std_logic_vector(9 downto 0);
	signal addrb0: std_logic_vector(9 downto 0);
	signal addrb1: std_logic_vector(9 downto 0);
	signal addrb2: std_logic_vector(9 downto 0);
	signal datab: std_logic_vector(35 downto 0);

	signal web: std_logic; --mine
	signal web0: std_logic; --mine
	signal web1: std_logic; --mine
	signal web2: std_logic; --mine
	signal uaddrb: unsigned(9 downto 0):=b"0000000000"; --mine
	signal pio_count: unsigned(10 downto 0):=b"00000000000"; --mine
	signal pio_state: std_logic; --mine
	signal sr0: std_logic_vector(11 downto 0); --mine
	signal sr1: std_logic_vector(11 downto 0); --mine
	signal sr2: std_logic_vector(11 downto 0); --mine
	signal sr3: std_logic_vector(35 downto 0); --mine
	signal datab_i_0: std_logic_vector(35 downto 0); --mine
	signal datab_i_1: std_logic_vector(35 downto 0); --mine
	signal datab_i_2: std_logic_vector(35 downto 0); --mine
	signal trigflag: std_logic; --mine
	signal trigcount: unsigned(9 downto 0):=b"0000000000"; --mine
	
	signal re_buf: unsigned(1 downto 0);
	signal wr_buf: unsigned(1 downto 0);
	
	signal clkfb:    std_logic;
	signal clkfx:    std_logic;
	signal hcount:   unsigned(9 downto 0);
	signal vcount:   unsigned(9 downto 0);
	signal blank:    std_logic;
	signal frame:    std_logic;
	signal obj1_red: std_logic_vector(1 downto 0);
	signal obj1_grn: std_logic_vector(1 downto 0);
	signal obj1_blu: std_logic_vector(1 downto 0);
    
    signal scaled_hcount: unsigned(9 downto 0);
    signal scaled_vcount: unsigned(11 downto 0);

    --vertical gain section
    signal vertical_gain: unsigned(6 downto 0):=to_unsigned(9,7); 
    signal vertical_gain_index: unsigned(3 downto 0):=to_unsigned(4,4); --there can be 8 different gains
	type gain_lookup_table is array (7 downto 0) of unsigned(6 downto 0);
	signal gain : gain_lookup_table;
	signal btn0_0: std_logic; 
    signal btn0_1: std_logic;
    signal btn0_2: std_logic;
    signal btn1_0: std_logic;
    signal btn1_1: std_logic;
    signal btn1_2: std_logic;
    signal btn0_free: std_logic:='0';
    signal btn1_free: std_logic:='0';
    
    --horizontal gain section
    signal horizontal_gain: unsigned(6 downto 0):=to_unsigned(1,7); --might not need to go as extreme as 64 on horizontal gain.
    signal hgain_counter: unsigned(6 downto 0):=to_unsigned(0,7);
    type hgain_machine is (hgain, no_hgain);
	signal FSM_hgain: hgain_machine;
    --signal h_trig: std_logic:='0';
    
    signal v_enc_clk_1: std_logic;
    signal v_enc_clk_2: std_logic;
    signal v_enc_clk_3: std_logic;
    signal v_enc_cw_cnt: unsigned(9 downto 0);
    signal v_enc_ccw_cnt: unsigned(9 downto 0);
    signal v_enc_cw_free: std_logic;
    signal v_enc_ccw_free: std_logic;
    signal v_enc_d_1: std_logic;
    signal v_enc_d_2: std_logic;
    signal v_enc_d_3: std_logic;
    signal v_off_plus: unsigned(11 downto 0);
    signal v_off_minus: unsigned(11 downto 0);
    
    
    --trigcount: what trigger index you're at
    signal pre_trig: unsigned(9 downto 0):=b"0000000000"; --how much data you want to show before the trigger
    signal post_trig: unsigned(9 downto 0):=b"1001111111"; --start at 639, how much data you want to show after the trigger
    
    
	type FSM_Type is (S0, S1, S2); --mine
	signal FSM_enc: FSM_type:= S0;
	signal enc_btn_free: std_logic:= '1';
	signal enc_b_0: std_logic;
    signal enc_b_1: std_logic;
    signal enc_b_2: std_logic;
    signal enc_b_3: std_logic;
    
    signal thrsh_lvl: unsigned(11 downto 0):= b"100000000000"; --set to 2048 to before we want to adjust it
    signal scaled_thrsh: unsigned(11 downto 0);
    signal scaled_samples: unsigned(16 downto 0);
    signal timeout: std_logic:='0';
    signal timeout_counter: unsigned(27 downto 0):=to_unsigned(0,28);  
	
begin
--	cmt1: lab05_cmt port map(clk_i=>clk,clk_o=>fclk);
	adc: lab05_adc port map(clk_i=>clkfx,vaux5n_i=>vaux5_n,vaux5p_i=>vaux5_p,
		rdy_o=>rdy,data_o=>datab(11 downto 0));
	ram0: lab05_ram port map(clka_i=>clkfx,wea_i=>'0',addra_i=>addra0,
		dataa_i=>(others=>'0'),dataa_o=>dataa0,clkb_i=>clkfx,
		web_i=>web0,addrb_i=>addrb0,datab_i=>datab_i_0,datab_o=>open); --mine
	ram1: lab05_ram port map(clka_i=>clkfx,wea_i=>'0',addra_i=>addra1,
		dataa_i=>(others=>'0'),dataa_o=>dataa1,clkb_i=>clkfx,
		web_i=>web1,addrb_i=>addrb1,datab_i=>datab_i_1,datab_o=>open); --mine
	ram2: lab05_ram port map(clka_i=>clkfx,wea_i=>'0',addra_i=>addra2,
		dataa_i=>(others=>'0'),dataa_o=>dataa2,clkb_i=>clkfx,
		web_i=>web2,addrb_i=>addrb2,datab_i=>datab_i_2,datab_o=>open); --mine
		
		gain <= (to_unsigned(1,7), to_unsigned(2,7), to_unsigned(3,7), to_unsigned(4,7), to_unsigned(9,7), to_unsigned(16,7), to_unsigned(32,7), to_unsigned(64,7)); --setting my gain

		addrb <= std_logic_vector(uaddrb);
	--	addrb1 <= std_logic_vector(uaddrb1);
	--	addrb2 <= std_logic_vector(uaddrb2);
	
pio31<= pio_state;
		
	tx<='1';

	------------------------------------------------------------------
	-- Clock management tile
	--
	-- Input clock: 12 MHz
	-- Output clock: 25.2 MHz
	--
	-- CLKFBOUT_MULT_F: 50.875
	-- CLKOUT0_DIVIDE_F: 24.250
	-- DIVCLK_DIVIDE: 1
	------------------------------------------------------------------
	cmt: MMCME2_BASE generic map (
		-- Jitter programming (OPTIMIZED, HIGH, LOW)
		BANDWIDTH=>"OPTIMIZED",
		-- Multiply value for all CLKOUT (2.000-64.000).
		CLKFBOUT_MULT_F=>50.875,
		-- Phase offset in degrees of CLKFB (-360.000-360.000).
		CLKFBOUT_PHASE=>0.0,
		-- Input clock period in ns to ps resolution (i.e. 33.333 is 30 MHz).
		CLKIN1_PERIOD=>83.333,
		-- Divide amount for each CLKOUT (1-128)
		CLKOUT1_DIVIDE=>1,
		CLKOUT2_DIVIDE=>1,
		CLKOUT3_DIVIDE=>1,
		CLKOUT4_DIVIDE=>1,
		CLKOUT5_DIVIDE=>1,
		CLKOUT6_DIVIDE=>1,
		-- Divide amount for CLKOUT0 (1.000-128.000):
		CLKOUT0_DIVIDE_F=>24.250,
		-- Duty cycle for each CLKOUT (0.01-0.99):
		CLKOUT0_DUTY_CYCLE=>0.5,
		CLKOUT1_DUTY_CYCLE=>0.5,
		CLKOUT2_DUTY_CYCLE=>0.5,
		CLKOUT3_DUTY_CYCLE=>0.5,
		CLKOUT4_DUTY_CYCLE=>0.5,
		CLKOUT5_DUTY_CYCLE=>0.5,
		CLKOUT6_DUTY_CYCLE=>0.5,
		-- Phase offset for each CLKOUT (-360.000-360.000):
		CLKOUT0_PHASE=>0.0,
		CLKOUT1_PHASE=>0.0,
		CLKOUT2_PHASE=>0.0,
		CLKOUT3_PHASE=>0.0,
		CLKOUT4_PHASE=>0.0,
		CLKOUT5_PHASE=>0.0,
		CLKOUT6_PHASE=>0.0,
		-- Cascade CLKOUT4 counter with CLKOUT6 (FALSE, TRUE)
		CLKOUT4_CASCADE=>FALSE,
		-- Master division value (1-106)
		DIVCLK_DIVIDE=>1,
		-- Reference input jitter in UI (0.000-0.999).
		REF_JITTER1=>0.0,
		-- Delays DONE until MMCM is locked (FALSE, TRUE)
		STARTUP_WAIT=>FALSE
	) port map (
		-- User Configurable Clock Outputs:
		CLKOUT0=>clkfx,  -- 1-bit output: CLKOUT0
		CLKOUT0B=>open,  -- 1-bit output: Inverted CLKOUT0
		CLKOUT1=>open,   -- 1-bit output: CLKOUT1
		CLKOUT1B=>open,  -- 1-bit output: Inverted CLKOUT1
		CLKOUT2=>open,   -- 1-bit output: CLKOUT2
		CLKOUT2B=>open,  -- 1-bit output: Inverted CLKOUT2
		CLKOUT3=>open,   -- 1-bit output: CLKOUT3
		CLKOUT3B=>open,  -- 1-bit output: Inverted CLKOUT3
		CLKOUT4=>open,   -- 1-bit output: CLKOUT4
		CLKOUT5=>open,   -- 1-bit output: CLKOUT5
		CLKOUT6=>open,   -- 1-bit output: CLKOUT6
		-- Clock Feedback Output Ports:
		CLKFBOUT=>clkfb,-- 1-bit output: Feedback clock
		CLKFBOUTB=>open, -- 1-bit output: Inverted CLKFBOUT
		-- MMCM Status Ports:
		LOCKED=>open,    -- 1-bit output: LOCK
		-- Clock Input:
		CLKIN1=>clk,   -- 1-bit input: Clock
		-- MMCM Control Ports:
		PWRDWN=>'0',     -- 1-bit input: Power-down
		RST=>'0',        -- 1-bit input: Reset
		-- Clock Feedback Input Port:
		CLKFBIN=>clkfb  -- 1-bit input: Feedback clock
	);

	------------------------------------------------------------------
	-- VGA display counters
	--
	-- Pixel clock: 25.175 MHz (actual: 25.2 MHz)
	-- Horizontal count (active low sync):
	--     0 to 639: Active video
	--     640 to 799: Horizontal blank
	--     656 to 751: Horizontal sync (active low)
	-- Vertical count (active low sync):
	--     0 to 479: Active video
	--     480 to 524: Vertical blank
	--     490 to 491: Vertical sync (active low)
	------------------------------------------------------------------
	--process(clkfx)
	process(clkfx, uaddrb, pio_count, datab, btn, thrsh, trigflag, sr0, sr1, sr2)
	begin
		if rising_edge(clkfx) then --if the vga clock is rising 
			-- Pixel position counters
			if (hcount>=to_unsigned(799,10)) then --rollover horizontal count
				hcount<=(others=>'0');
				if (vcount>=to_unsigned(524,10)) then --rollover vertical count
					vcount<=(others=>'0');
				else
					vcount<=vcount+1;
				end if;
			else
				hcount<=hcount+1;
			end if;
			-- Sync, blank and frame
			if (hcount>=to_unsigned(656,10)) and
				(hcount<=to_unsigned(751,10)) then
				hsync<='0'; --active low hsync value
			else
				hsync<='1'; --otherwise hsync is hi
			end if;
			if (vcount>=to_unsigned(490,10)) and
				(vcount<=to_unsigned(491,10)) then
				vsync<='0'; --active low vsync value
			else
				vsync<='1';
			end if;
			if (hcount>=to_unsigned(640,10)) or  --when not in active video, be in blanking mode
				(vcount>=to_unsigned(480,10)) then
				blank<='1';
			else
				blank<='0';
			end if;
			if (hcount=to_unsigned(640,10)) and --when we get to the last pixel, one frame has passed, and we set frame high for one clock
				(vcount=to_unsigned(479,10)) then
				frame<='1';
			else
				frame<='0';
			end if;
					

			
---Ram buffering- Ram assigner
		
		case wr_buf is        --write buffer
		  when b"00" =>
		      web0 <= web;
		      web1 <= '0'; --make other ram blocks unwriteable when we switch to the new one
		      web2 <= '0';
		      addrb0 <= std_logic_vector(uaddrb);
		      datab_i_0 <= sr3;
		  when b"01" =>
		      web1 <= web;
		      web0 <= '0';
		      web2 <= '0';
		      addrb1 <= std_logic_vector(uaddrb);
		      datab_i_1 <= sr3;
		  when b"10" =>
		      web2 <= web;
		      web1 <= '0';
		      web0 <= '0';
		      addrb2 <= std_logic_vector(uaddrb);
		      datab_i_2 <= sr3;
		  when others =>
		      null;
		end case;
		
		case re_buf is            --read buffer
		  when b"00" =>
		      addra0 <= addra;
		      dataa <= dataa0;
		  when b"01" =>
		      addra1 <= addra;
		      dataa <= dataa1;
		  when b"10" =>
		      addra1 <= addra;
		      dataa <= dataa1;
		  when others =>
		      null;
		end case;

    --VGA- drawing
 
    scaled_thrsh <= 480 - (thrsh_lvl/vertical_gain) - v_off_plus + v_off_minus;
    scaled_samples <= to_unsigned(samples,17)/horizontal_gain;
 
    addra <= std_logic_vector(hcount); --we read the Nth number in ram
    scaled_vcount<= 480-(unsigned(dataa(11 downto 0 ))/vertical_gain) - v_off_plus + v_off_minus;    --We scale the 12 bit number down, so 0-4096 --> 0-455 (less than 480 vert pix),
    --and flip it so 3.3V is  pixel 0, which is the top of the screen
    if (vcount = scaled_vcount) then --and (v_enc_cw_free = '1' or v_enc_ccw_free = '1')) then    --if the current row is the same value as the scaled version
        blank<='0';         -- don't blank, set the colors
        obj1_red <= b"11";
    --    obj1_blu <= b"11";
    elsif vcount = scaled_thrsh then
        blank<='0';         -- don't blank, set the colors
        obj1_blu <= b"11";
    else
        blank<='1';     --otherwise blank, no color
    end if; 

--        vga_draw_cnt <= vga_draw_cnt + 1;
	
	
		
		if frame = '1' then
		
		    --Ram buffering- read buffer logic

        if uaddrb = samples then -- =639
            re_buf <= wr_buf;
        else
            re_buf <= (wr_buf - 1) mod 3;
        end if;
    else
        re_buf <= re_buf;
		
		
		end if; --end of frame
		
        

        
        --drive neighboring pin in square wave
        if(pio_count = 1039) then --if it rolls over
            pio_count<= b"00000000000";
            if (pio_state = '0') then
                pio_state <= '1';
            else
                pio_state <= '0';
            end if;
        else --if it does not rollover, increment
            pio_count<= pio_count + 1;
        end if;
         
         
         
         
       if timeout_counter=to_unsigned(timeout_thresh,28) then
           timeout_counter<=to_unsigned(0,28);
           timeout<='1';
       else
           if FSM_hgain = no_hgain and timeout = '0' then       --if we're not triggering and 
                timeout_counter<=timeout_counter+1;
           end if;
       end if;
       
       --basically, if the trigger is high, then either increment trigcount normally, or slower.
   
        sr0 <= datab(11 downto 0); --Shift Register gets data from ADC 
        sr1 <= sr0; --Data from one older clock cycle, used for triggering comparison
        
       case FSM_hgain is
           when no_hgain =>
            if (unsigned(sr1) <= thrsh_lvl and unsigned(sr0) >= thrsh_lvl) or timeout='1' then
                FSM_hgain <= hgain;
            end if;
           when hgain =>
                              
            if(uaddrb = samples) then --Collect samples, then rollover the count and reset the flag
                    FSM_hgain <= no_hgain;
                    timeout <= '0';
                    uaddrb <= b"0000000000";
                    if re_buf = (wr_buf + 1)mod 3 then
                        wr_buf <= (wr_buf + 2)mod 3;
                    else
                        wr_buf <= (wr_buf + 1)mod 3;
                    end if;
               else --if not at max
               
                   if (hgain_counter = horizontal_gain-1) then
                    uaddrb <= uaddrb+1; --the ADC sample will go to the very next address
                    hgain_counter <= to_unsigned(0,7);
                   else
                    --drop the ADC sample
                    hgain_counter <= hgain_counter+1;
                   end if;
                   
                   sr2 <= sr1;
                   sr3(11 downto 0) <= sr2; --Shift Register sends data to Ram Block
                   --trigcount <= trigcount + 1;
               end if;       
                   
       end case;     
       


    --Vert position encoder
    v_enc_d_1 <= v_enc_d;
    v_enc_d_2 <= v_enc_d_1;
    v_enc_d_3 <= v_enc_d_2;
    
    v_enc_clk_1 <= v_enc_clk;
    v_enc_clk_2 <= v_enc_clk_1;
    v_enc_clk_3 <= v_enc_clk_2;
    
    --saturation counter
    
    if v_enc_d_3 > v_enc_d_1 then  --if falling edge
        v_enc_cw_free<= '1'; --flag allows for cw to be read
        v_enc_ccw_free<= '1'; --flag allows for ccw to be read
    end if;
    
--    if v_enc_clk_3 > v_enc_clk_1 then  --if falling edge
--        v_enc_cw_free<= '1'; --flag allows for cw to be read
--        v_enc_ccw_free<= '1'; --flag allows for ccw to be read
--    end if;
    
    if (v_enc_clk_3='1') then --if clk is hi after d falls, start CCW counting
        if v_enc_ccw_free = '1' then
            if (v_enc_ccw_cnt < 300) then --if we are less than max count
                v_enc_ccw_cnt<=v_enc_ccw_cnt+1;     --increment
            else --if we hit max count
              v_enc_ccw_cnt <= b"0000000000";       --reset both counters
              v_enc_cw_cnt <= b"0000000000";
              v_enc_ccw_free <= '0';                --reset both flags
              v_enc_cw_free <= '0';
              
              
              case FSM_enc is
                when S0=>       --vertical position
             --   if v_off_plus > 128/vertical_gain then  -- if we don't have a positive to take away from
                if v_off_plus > 0 then  -- if we don't have a positive to take away from
           --         v_off_minus <= v_off_minus + 128/vertical_gain; --move down
                    v_off_minus <= v_off_minus + 1; --move down
                else
                    --v_off_plus <= v_off_plus - 128/vertical_gain;   --move less up
                    v_off_plus <= v_off_plus - 1;   --move less up
                end if;
                when S1=>   --horizontal position
                   if post_trig < samples then
                      pre_trig <= pre_trig - 1;
                      post_trig <= post_trig + 1;
                  else        --if we hit max, then don't move horiz
                      pre_trig <= pre_trig;
                      post_trig <= post_trig;
                  end if;
                when S2=>   --trigger position
                    if thrsh_lvl > 10 then -- 0 + 10
                        thrsh_lvl <= thrsh_lvl - 10;
                    else
                        thrsh_lvl <= thrsh_lvl;
                    end if;
              end case;
                
                
                
                
            end if;
        end if;
    --elsif (v_enc_clk_3='0') then    --if clk is low after d falls, start cw counting
    else
        if v_enc_cw_free = '1' then
            if (v_enc_cw_cnt < 300) then --if the button is being pressed, and we aren't at max, increase dbcount CW counting
                v_enc_cw_cnt<=v_enc_cw_cnt+1;
            else --if we hit max count
              v_enc_cw_cnt <= b"0000000000";
              v_enc_ccw_cnt <= b"0000000000";
              v_enc_cw_free <= '0';
              v_enc_ccw_free <= '0';
              
              
              case FSM_enc is
                when S0=>       --vertical position
            --    if v_off_minus > 128/vertical_gain then
                if v_off_minus > 0 then
                --    v_off_plus <= v_off_plus + 128/vertical_gain; --move up
                     v_off_plus <= v_off_plus + 1; --move up
                else
                    --v_off_minus <= v_off_minus - 128/vertical_gain; --move less down
                    v_off_minus <= v_off_minus - 1;
                end if;
                when S1=>   --horizontal position
                  if pre_trig < samples then
                      pre_trig <= pre_trig + 1;
                      post_trig <= post_trig - 1;
                  else --if we hit max, then don't move horiz
                      pre_trig <= pre_trig;
                      post_trig <= post_trig;
                  end if;
                when S2=>   --trigger position
                    if thrsh_lvl < 4085 then -- 4095 - 10 might need to change this range
                        thrsh_lvl <= thrsh_lvl + 10;
                    else
                        thrsh_lvl <= thrsh_lvl;
                    end if;
              end case;
              
              
            end if;
        end if;
    end if;
    
        btn0_0 <= btn(0);
        btn0_1 <= btn0_0;
        btn0_2 <= btn0_1;
        btn1_0 <= btn(1);
        btn1_1 <= btn1_0;
        btn1_2 <= btn1_1;
        if (btn0_2='1') then
            if(btn0_free='1') then
               if (vertical_gain_index > 0) then
                   vertical_gain_index<=vertical_gain_index-1;
               end if;
               btn0_free<='0';
            end if;
        else
            btn0_free<='1';
        end if;
    
    if (btn1_2='1') then
        if(btn1_free='1') then
            if (vertical_gain_index < 7) then
                vertical_gain_index<=vertical_gain_index+1;
		    end if;
		    btn1_free<='0';
	    end if;
	else
	   btn1_free<='1';
	end if;
	    
    vertical_gain<=gain(to_integer(vertical_gain_index));


--Encoder Button Stuff
enc_b_0 <= enc_btn;
enc_b_1 <= enc_b_0;
enc_b_2 <= enc_b_1;
enc_b_3 <= enc_b_2;

if enc_b_3 = '0' then
    if enc_btn_free = '1' then
      case FSM_enc is
        when S0=>       --vertical position
            FSM_enc <= S1;
        when S1=>   --horizontal position
            FSM_enc <= S2;
        when S2=>   --trigger position
            FSM_enc <= S0;
      end case;
      enc_btn_free <= '0';
    end if;
else
    FSM_enc <= FSM_enc;
    enc_btn_free <= '1';
end if;

--thrsh_lvl <= unsigned(thrsh);

end if; --end of rising edge


end process;
	
	
--	process(clkfx, uaddrb, pio_count, datab, btn, thrsh, trigcount, trigflag, sr0, sr1, sr2)
--begin
--	if rising_edge(clkfx) then

         
         
--   end if; --rising edge if
   
 --  sr3(11 downto 0) <= datab(11 downto 0); --Square wave test
   
   
--end process;



	datab(35 downto 12)<=(others=>'0');
	

	------------------------------------------------------------------
	-- VGA output with blanking
	------------------------------------------------------------------
	red<=b"00" when blank='1' else obj1_red; --assigning the color output to our internal color variable, otherwise 0 when blanking
	green<=b"00" when blank='1' else obj1_grn;
	blue<=b"00" when blank='1' else obj1_blu;
	




end arch;
