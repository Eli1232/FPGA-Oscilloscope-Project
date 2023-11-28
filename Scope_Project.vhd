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
		btn:     in  std_logic;
		pio31:   out std_logic;
		v_enc_d: in std_logic;  --pin 47 encoder DT 
		v_enc_clk: in std_logic;   --pin 48 encoder CLK
		h_enc_d: in std_logic;  --pin 45 encoder DT **check if these are the right direction
		h_enc_clk: in std_logic   --pin 46 encoder CLK
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
	
		constant samples: natural:=200;
	signal fclk:  std_logic;
	signal rdy:   std_logic;
	signal thrsh: std_logic_vector(11 downto 0):= b"101110111000"; --set to 3000 to before we want to adjust it
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
	signal trigcount: unsigned(7 downto 0):=b"00000000"; --mine
	
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
    signal pre_trig: unsigned(7 downto 0):=b"00000000"; --how much data you want to show before the trigger
    signal post_trig: unsigned(7 downto 0):=b"11000111"; --start at 199, how much data you want to show after the trigger
    
  --  signal vga_draw_cnt: unsigned(7 downto 0);
	
	--    if (hcount>=to_unsigned(220,10)) and
--    (hcount<=to_unsigned(420,10)) and 
--    (vcount>=to_unsigned(140,10)) and 
--    (vcount<=to_unsigned(340,10)) then
	
begin
	--cmt: lab05_cmt port map(clk_i=>clk,clk_o=>fclk);
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
	process(clkfx, uaddrb, pio_count, datab, btn, thrsh, trigcount, trigflag, sr0, sr1, sr2)
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
		
	
    --Ram buffering- read buffer logic

    if addra = b"0011000111"  then  --  = 199
        if trigcount >= b"0011000111" then -- =199
            re_buf <= wr_buf;
        else
            re_buf <= (wr_buf - 1) mod 3;
        end if;
    else
        re_buf <= re_buf;
    end if; 
    
    
    --VGA- drawing
 
    if hcount mod 3 = b"0000000000" then --every 3rd column, we want to draw a pixel of one of our 200 samples
        addra <= std_logic_vector(hcount/3); --we read the Nth number in ram
        scaled_vcount<= 480-(unsigned(dataa(11 downto 0 ))/9) - v_off_plus + v_off_minus;    --We scale the 12 bit number down, so 0-4096 --> 0-455 (less than 480 vert pix),
        --and flip it so 3.3V is  pixel 0, which is the top of the screen
        if (vcount = scaled_vcount) then    --if the current row is the same value as the scaled version
            blank<='0';         -- don't blank, set the colors
            obj1_red <= b"11";
        --    obj1_blu <= b"11";
        else
            blank<='1';     --otherwise blank, no color
        end if;
    else 
        blank<='1';     --otherwise blank, no color
    end if;
--        vga_draw_cnt <= vga_draw_cnt + 1;
	
	
		
		if frame = '1' then
		
		end if; --end of frame
		
        
        --ADC- writing values to RAM NORMAL
            if (rdy = '1') then --if the ADC has a value to send
                web <= '1';		--enable writing to RAMB
                if ((trigcount = 0)) then --handle rollover addresses 0-199  
                    uaddrb <= b"0000000000";
                else
                    uaddrb<= uaddrb + 1; --if not at max value, increment
                end if;
            else
                uaddrb<= uaddrb; --if there is no new ADC value, write to the old address
                web <= '1';
            end if;
        
        
--                --ADC- writing values to RAM with horizontal shift adjustment
--            if (rdy = '1') then --if the ADC has a value to send
--                web <= '1';		--enable writing to RAMB
--                if ((trigflag = '1' and trigcount = 0)) then --handle rollover addresses 0-199  **normal one
--                    uaddrb <= b"0000000000";
--                else
--                    uaddrb<= uaddrb + 1; --if not at max value, increment
--                end if;
--            else
--                uaddrb<= uaddrb; --if there is no new ADC value, write to the old address
--                web <= '1';
--            end if;
        
        
        
        
        
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
         
     
     
     
         
         
  --triggering normal version
--   if(rdy = '1' and btn = '1') then
     if rdy = '1' then
       sr0 <= datab(11 downto 0); --Shift Register gets data from ADC 
       sr1 <= sr0; --Data from one older clock cycle, used for triggering comparison
       if(unsigned(sr1) <= unsigned(thrsh) and unsigned(sr0) >= unsigned(thrsh)) then --If signal rose above thrsh, then trigger
            trigflag <= '1'; 
       else
            trigflag <= trigflag;
       end if;
       if(trigflag = '1') then
--           if(trigcount = 0) then --Set the ram address to 0 so it's on the left of the screen
--                uaddrb <= b"0000000000";
--           end if;
           sr2 <= sr1;
           sr3(11 downto 0) <= sr2; --Shift Register sends data to Ram Block
           trigcount <= trigcount + 1;
       end if;
       
       
--         --triggering with horizontal shift
--        if rdy = '1' then
--            sr0 <= datab(11 downto 0); --Shift Register gets data from ADC 
--            sr1 <= sr0; --Data from one older clock cycle, used for triggering comparison
--            if trigcount < pre_trig then 
--                sr2 <= sr1;
--                sr3(11 downto 0) <= sr2; --Shift Register sends data to Ram Block
--                trigcount <= trigcount + 1;
--            elsif(unsigned(sr1) <= unsigned(thrsh) and unsigned(sr0) >= unsigned(thrsh)) then --If signal rose above thrsh, then trigger
--                trigflag <= '1'; 
--            else 
--                trigflag <= trigflag;         
--            end if;
--       if(trigflag = '1') then
----           if(trigcount = 0) then --Set the ram address to 0 so it's on the left of the screen
----                uaddrb <= b"0000000000";
----           end if;
--           sr2 <= sr1;
--           sr3(11 downto 0) <= sr2; --Shift Register sends data to Ram Block
--           trigcount <= trigcount + 1;
--       end if;    
       
       
       
       ----Ram buffering- write buffer logic
       
       if(trigcount >= 199) then --Collect 200 samples, then rollover the count and reset the flag
            trigflag <= '0';
            trigcount <= b"00000000";
            if re_buf = (wr_buf + 1)mod 3 then
                wr_buf <= (wr_buf + 2)mod 3;
            else
                wr_buf <= (wr_buf + 1)mod 3;
            end if;
       end if;
    end if;   --end if rdy is 1


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
    
    if (v_enc_clk_3='1') then --if clk is hi after d falls, start ccw counting
        if v_enc_ccw_free = '1' then
            if (v_enc_ccw_cnt < 300) then --if we are less than max count
                v_enc_ccw_cnt<=v_enc_ccw_cnt+1;     --increment
            else --if we hit max count
              v_enc_ccw_cnt <= b"0000000000";       --reset both counters
              v_enc_cw_cnt <= b"0000000000";
              v_enc_ccw_free <= '0';                --reset both flags
              v_enc_cw_free <= '0';
                if v_off_plus = 0 then  -- if we don't have a positive to take away from
                    v_off_minus <= v_off_minus + 1; --move down
                else
                    v_off_plus <= v_off_plus - 1;   --move less up
                end if;
            end if;
        end if;
    --elsif (v_enc_clk_3='0') then    --if clk is low after d falls, start cw counting
    else
        if v_enc_cw_free = '1' then
            if (v_enc_cw_cnt < 300) then --if the button is being pressed, and we aren't at max, increase dbcount
                v_enc_cw_cnt<=v_enc_cw_cnt+1;
            else --if we hit max count
              v_enc_cw_cnt <= b"0000000000";
              v_enc_ccw_cnt <= b"0000000000";
              v_enc_cw_free <= '0';
              v_enc_ccw_free <= '0';
                if v_off_minus = 0 then
                    v_off_plus <= v_off_plus + 1; --move up
                else
                    v_off_minus <= v_off_minus - 1; --move less down
                end if;
            end if;
        end if;
    end if;
    
    
    
    
--        if v_enc_clk_3 = '0' then --cw
--            if v_off_minus = 0 then
--                v_off_plus <= v_off_plus + 1; --move up
--            else
--                v_off_minus <= v_off_minus - 1; --move less down
--            end if;
--        else
--            if v_off_plus = 0 then  --ccw
--                v_off_minus <= v_off_minus + 1; --move down
--            else
--                v_off_plus <= v_off_plus - 1;   --move less up
--            end if;
--    end if; 	



--    --Horiz position encoder
--    --**********need to change all these variables
----    v_pos_cw_1 <= v_pos_cw;
----    v_pos_cw_2 <= v_pos_cw_1;
----    v_pos_cw_3 <= v_pos_cw_2;
    
----    v_pos_ccw_1 <= v_pos_ccw;
----    v_pos_ccw_2 <= v_pos_ccw_1;
----    v_pos_ccw_3 <= v_pos_ccw_2;
    
--    if v_pos_cw_3 < v_pos_cw_2 then  --if rising edge
--        if v_pos_ccw_3 = '0' then --if CW?
--            if pre_trig < 199 then
--                pre_trig <= pre_trig + 1;
--                post_trig <= post_trig - 1;
--            else
--                null;
--            end if;
--        else
--            if post_trig < 199 then
--                pre_trig <= pre_trig - 1;
--                post_trig <= post_trig + 1;
--            else
--                null;
--            end if;
--    end if; 	
--    end if;



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
