library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;
 
entity MIST_Toplevel is
	port
	(
		CLOCK_27		:	 in std_logic_vector(1 downto 0);
		
		LED			: 	out std_logic;

		UART_TX		:	 out STD_LOGIC;
		UART_RX		:	 in STD_LOGIC;

		SDRAM_DQ		:	 inout std_logic_vector(15 downto 0);
		SDRAM_A	:	 out std_logic_vector(12 downto 0);
		SDRAM_DQMH	:	 out STD_LOGIC;
		SDRAM_DQML	:	 out STD_LOGIC;
		SDRAM_nWE	:	 out STD_LOGIC;
		SDRAM_nCAS	:	 out STD_LOGIC;
		SDRAM_nRAS	:	 out STD_LOGIC;
		SDRAM_nCS	:	 out STD_LOGIC;
		SDRAM_BA		:	 out std_logic_vector(1 downto 0);
		SDRAM_CLK	:	 out STD_LOGIC;
		SDRAM_CKE	:	 out STD_LOGIC;

		SPI_DO	: inout std_logic;
		SPI_DI	: in std_logic;
		SPI_SCK		:	 in STD_LOGIC;
		SPI_SS2		:	 in STD_LOGIC; -- FPGA
		SPI_SS3		:	 in STD_LOGIC; -- OSD
		SPI_SS4		:	 in STD_LOGIC; -- "sniff" mode
		CONF_DATA0  : in std_logic; -- SPI_SS for user_io

		VGA_HS		:	buffer STD_LOGIC;
		VGA_VS		:	buffer STD_LOGIC;
		VGA_R		:	 out unsigned(5 downto 0);
		VGA_G		:	 out unsigned(5 downto 0);
		VGA_B		:	 out unsigned(5 downto 0);

		AUDIO_L : out std_logic;
		AUDIO_R : out std_logic
	);
END entity;

architecture rtl of MIST_Toplevel is

signal reset : std_logic;
signal reset_d : std_logic;
signal pll_locked : std_logic;
signal MCLK      : std_logic;
signal memclk      : std_logic;

-- "FLASH"
signal romwr_req : std_logic := '0';
signal romwr_ack : std_logic;
signal romwr_we  : std_logic := '1';
signal romwr_a : unsigned(21 downto 1);
signal romwr_d : std_logic_vector(15 downto 0);
signal romwr_q : std_logic_vector(15 downto 0);

signal romrd_req : std_logic := '0';
signal romrd_ack : std_logic;
signal romrd_a : std_logic_vector(21 downto 3);
signal romrd_q : std_logic_vector(63 downto 0);

-- 68000 RAM
signal ram68k_req : std_logic;
signal ram68k_ack : std_logic;
signal ram68k_we : std_logic;
signal ram68k_a : std_logic_vector(15 downto 1);
signal ram68k_d : std_logic_vector(15 downto 0);
signal ram68k_q : std_logic_vector(15 downto 0);
signal ram68k_l_n : std_logic;
signal ram68k_u_n : std_logic;

-- VRAM
signal vram_req : std_logic;
signal vram_ack : std_logic;
signal vram_we : std_logic;
signal vram_a : std_logic_vector(15 downto 1);
signal vram_d : std_logic_vector(15 downto 0);
signal vram_q : std_logic_vector(15 downto 0);
signal vram_l_n : std_logic;
signal vram_u_n : std_logic;

-- VDP Video Output
signal VDP_RED		: std_logic_vector(3 downto 0);
signal VDP_GREEN	: std_logic_vector(3 downto 0);
signal VDP_BLUE	: std_logic_vector(3 downto 0);
signal VDP_VS_N	: std_logic;
signal VDP_HS_N	: std_logic;

signal VDP_VGA_RED	: std_logic_vector(3 downto 0);
signal VDP_VGA_GREEN	: std_logic_vector(3 downto 0);
signal VDP_VGA_BLUE	: std_logic_vector(3 downto 0);
signal VDP_VGA_VS_N	: std_logic;
signal VDP_VGA_HS_N	: std_logic;

-- NTSC/RGB Video Output
signal RED			: std_logic_vector(7 downto 0);
signal GREEN		: std_logic_vector(7 downto 0);
signal BLUE			: std_logic_vector(7 downto 0);		
signal VS_N			: std_logic;
signal HS_N			: std_logic;

-- VGA Video Output
signal VGA_RED			: std_logic_vector(7 downto 0);
signal VGA_GREEN		: std_logic_vector(7 downto 0);
signal VGA_BLUE		: std_logic_vector(7 downto 0);		
signal VGA_VS_N		: std_logic;
signal VGA_HS_N		: std_logic;

-- current video signal (switchable between TV and VGA)
signal vga_red_i : std_logic_vector(7 downto 0);
signal vga_green_i : std_logic_vector(7 downto 0);
signal vga_blue_i	: std_logic_vector(7 downto 0);		
signal vga_vsync_i : std_logic;
signal vga_hsync_i : std_logic;
signal vga_red_o : std_logic_vector(7 downto 0);
signal vga_green_o : std_logic_vector(7 downto 0);
signal vga_blue_o	: std_logic_vector(7 downto 0);
signal vga_window : std_logic;

signal audiol : std_logic_vector(15 downto 0);
signal audior : std_logic_vector(15 downto 0);

-- user_io
signal buttons: std_logic_vector(1 downto 0);
signal status:  std_logic_vector(7 downto 0);
signal joy_0: std_logic_vector(7 downto 0);
signal joy_1: std_logic_vector(7 downto 0);
signal joy_2: std_logic_vector(7 downto 0);
signal joy_3: std_logic_vector(7 downto 0);
signal joy_4: std_logic_vector(7 downto 0);
signal joy_ana_0: std_logic_vector(15 downto 0);
signal joy_ana_1: std_logic_vector(15 downto 0);
signal txd:     std_logic;
signal par_out_data: std_logic_vector(7 downto 0);
signal par_out_strobe: std_logic;
signal joya: std_logic_vector(7 downto 0);
signal joyb: std_logic_vector(7 downto 0);
signal joya_swap: std_logic_vector(7 downto 0);
signal joyb_swap: std_logic_vector(7 downto 0);

-- signals to connect sd card emulation with io controller
signal sd_lba:  std_logic_vector(31 downto 0);
signal sd_rd:   std_logic;
signal sd_wr:   std_logic;
signal sd_ack:  std_logic;
signal sd_conf: std_logic;
signal sd_sdhc: std_logic;
signal sd_allow_sdhc: std_logic;
signal sd_allow_sdhcD: std_logic;
signal sd_allow_sdhcD2: std_logic;
signal sd_allow_sdhc_changed: std_logic;
-- data from io controller to sd card emulation
signal sd_data_in: std_logic_vector(7 downto 0);
signal sd_data_in_strobe:  std_logic;
signal sd_data_out: std_logic_vector(7 downto 0);
signal sd_data_out_strobe:  std_logic;

-- sd card emulation
signal spi_cs:	std_logic;
signal spi_clk:	std_logic;
signal spi_mosi:	std_logic;
signal spi_miso:	std_logic;

-- PS/2
signal ps2_clk : std_logic;
signal ps2counter : unsigned(10 downto 0);

-- PS/2 Keyboard
signal ps2k_clk_in : std_logic;
signal ps2k_dat_in : std_logic;
signal ps2k_clk_mix : std_logic;
signal ps2k_clk_out : std_logic;
signal ps2k_dat_out : std_logic;

-- PS/2 Mouse
signal ps2m_clk_in : std_logic;
signal ps2m_dat_in : std_logic;
signal ps2m_clk_mix : std_logic;
signal ps2m_clk_out : std_logic;
signal ps2m_dat_out : std_logic;

-- spi clock recovery
signal spirecoveryclock : std_logic;  -- High frequency clock
signal spisck_d : std_logic;
signal spirec : std_logic;
signal spirecoveredclock : std_logic;
signal pll2_locked : std_logic;

signal SDR_INIT_DONE	: std_logic;
signal PRE_RESET_N	: std_logic;

type bootStates is (BOOT_READ_1, BOOT_WRITE_1, BOOT_WRITE_2, BOOT_DONE);
signal bootState : bootStates := BOOT_READ_1;

signal host_reset_n : std_logic;
signal host_bootdone : std_logic;
signal rommap : std_logic_vector(1 downto 0);

signal boot_req : std_logic;
signal boot_ack : std_logic;
signal boot_data : std_logic_vector(15 downto 0);
signal FL_DQ : std_logic_vector(15 downto 0);

signal osd_window : std_logic;
signal osd_pixel : std_logic;

type romStates is (ROM_IDLE, ROM_READ);
signal romState : romStates := ROM_IDLE;

signal MASTER_VOLUME : std_logic_vector(2 downto 0);

signal gp1emu : std_logic_vector(7 downto 0);
signal gp2emu : std_logic_vector(7 downto 0);

signal SW : std_logic_vector(15 downto 0);

-- Sigma Delta audio
COMPONENT hybrid_pwm_sd
	PORT
	(
		clk		:	 IN STD_LOGIC;
		n_reset		:	 IN STD_LOGIC;
		din		:	 IN STD_LOGIC_VECTOR(15 DOWNTO 0);
		dout		:	 OUT STD_LOGIC
	);
END COMPONENT;

COMPONENT video_vga_dither
	GENERIC ( outbits : INTEGER := 4 );
	PORT
	(
		clk		:	 IN STD_LOGIC;
		hsync		:	 IN STD_LOGIC;
		vsync		:	 IN STD_LOGIC;
		vid_ena		:	 IN STD_LOGIC;
		iRed		:	 IN UNSIGNED(7 DOWNTO 0);
		iGreen		:	 IN UNSIGNED(7 DOWNTO 0);
		iBlue		:	 IN UNSIGNED(7 DOWNTO 0);
		oRed		:	 OUT UNSIGNED(outbits-1 DOWNTO 0);
		oGreen		:	 OUT UNSIGNED(outbits-1 DOWNTO 0);
		oBlue		:	 OUT UNSIGNED(outbits-1 DOWNTO 0)
	);
END COMPONENT;
  

component user_io 
	generic ( STRLEN : integer := 0 );
   port (
			  SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
           SPI_MISO : out std_logic;
           conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
           joystick_0 : out std_logic_vector(7 downto 0);
           joystick_1 : out std_logic_vector(7 downto 0);
           joystick_2 : out std_logic_vector(7 downto 0);
           joystick_3 : out std_logic_vector(7 downto 0);
           joystick_4 : out std_logic_vector(7 downto 0);
           joystick_analog_0 : out std_logic_vector(15 downto 0);
           joystick_analog_1 : out std_logic_vector(15 downto 0);
           status: out std_logic_vector(7 downto 0);
           switches : out std_logic_vector(1 downto 0);
           buttons : out std_logic_vector(1 downto 0);
			  sd_lba : in std_logic_vector(31 downto 0);
			  sd_rd : in std_logic;
			  sd_wr : in std_logic;
			  sd_ack : out std_logic;
			  sd_conf : in std_logic;
			  sd_sdhc : in std_logic;
			  sd_dout : out std_logic_vector(7 downto 0);
			  sd_dout_strobe : out std_logic;
			  sd_din : in std_logic_vector(7 downto 0);
			  sd_din_strobe : out std_logic;
           ps2_clk : in std_logic;
           ps2_kbd_clk : out std_logic;
           ps2_kbd_data : out std_logic;
           ps2_mouse_clk : out std_logic;
           ps2_mouse_data : out std_logic;
			  serial_data : in std_logic_vector(7 downto 0);
           serial_strobe : in std_logic
      );
  end component user_io;
  
component mist_console
	generic ( CLKFREQ : integer := 100 );
   port (  clk 	:	in std_logic;
           n_reset:	in std_logic;
           ser_in :	in std_logic;
           par_out_data :	out std_logic_vector(7 downto 0);
           par_out_strobe :	out std_logic
  );
  end component mist_console;

component sd_card
   port (  io_lba 	: out std_logic_vector(31 downto 0);
			  io_rd  	: out std_logic;
			  io_wr  	: out std_logic;
			  io_ack 	: in std_logic;
			  io_sdhc 	: out std_logic;
			  io_conf 	: out std_logic;
			  io_din 	: in std_logic_vector(7 downto 0);
			  io_din_strobe : in std_logic;
			  io_dout 	: out std_logic_vector(7 downto 0);
			  io_dout_strobe : in std_logic;

			  allow_sdhc : in std_logic;
			  
           sd_cs 		:	in std_logic;
           sd_sck 	:	in std_logic;
           sd_sdi 	:	in std_logic;
           sd_sdo 	:	out std_logic
  );
  end component sd_card;

begin


myrecoveryclock : entity work.SPIRecoveryClock
port map
(
	inclk0 => CLOCK_27(0),
	c0 => spirecoveryclock,
	locked => pll2_locked
);

process(spirecoveryclock)
begin
	if rising_edge(spirecoveryclock) then
		spisck_d <= SPI_SCK;	-- ~2.3ns
		spirec <= '0';
		spirecoveredclock <= '0';
		if (spirec='1' and (spisck_d='1' or SPI_SCK='1'))
			or (spirec='0' and (spisck_d='1' and SPI_SCK='1')) then
				spirec <= '1';  -- 6.9ns
				spirecoveredclock <= '1';  -- 6.9ns
		end if;
	end if;
end process;



  U00 : entity work.pll
    port map(
      inclk0 => CLOCK_27(0),	-- 27 MHz external
      c0     => MCLK,			-- 54 MHz internal
		c1     => open,
      c2     => memclk,			-- 108 Mhz
      c3     => SDRAM_CLK,		-- 108 Mhz external
		locked => pll_locked
    );

--SDRAM_A(12)<='0';

-- reset from IO controller
-- status bit 0 is always triggered by the i ocontroller on its own reset
-- button 1 is the core specfic button in the mists front
-- reset <= '0' when status(0)='1' or buttons(1)='1' or pll_locked='0' else '1';

process(MCLK)
begin
	if rising_edge(MCLK) then
		reset_d<=not (status(0) or status(2) or buttons(1)) and pll_locked and pll2_locked;
		reset<=reset_d;
	end if;
end process;

-- MCLK?
process(MCLK)
begin
--	ps2k_clk_mix <= ps2k_clk_in and (ps2_clk or ps2k_dat_out);
	ps2k_clk_mix <= ps2k_clk_in; -- and (ps2_clk or ps2k_dat_out);
	ps2m_clk_mix <= ps2m_clk_in; -- and (ps2_clk or ps2m_dat_out);
	if rising_edge(MCLK) then
		ps2counter<=ps2counter+1;
		if ps2counter=1200 then
			ps2_clk<=not ps2_clk;
			ps2counter<=(others => '0');
		end if;
	end if;
end process;

SDRAM_CKE <= '1';
SDRAM_nCS <= '0';
sdc : entity work.sdram_controller
	port map(
	clk			=> memclk,
	
	std_logic_vector(sd_data)	=> SDRAM_DQ,
	std_logic_vector(sd_addr)	=> SDRAM_A,
	sd_we_n							=> SDRAM_nWE,
	sd_ras_n							=> SDRAM_nRAS,
	sd_cas_n							=> SDRAM_nCAS,
	sd_ba_0							=> SDRAM_BA(0),
	sd_ba_1							=> SDRAM_BA(1),
	sd_ldqm							=> SDRAM_DQML,
	sd_udqm							=> SDRAM_DQMH,
		
	romwr_req	=> romwr_req,
	romwr_ack	=> romwr_ack,
	romwr_we 	=> romwr_we,
	romwr_a		=> std_logic_vector(romwr_a),
	romwr_d		=> romwr_d,
	romwr_q		=> romwr_q,
	
	romrd_req	=> romrd_req,
	romrd_ack	=> romrd_ack,
	romrd_a		=> romrd_a,
	romrd_q		=> romrd_q,

	ram68k_req	=> ram68k_req,
	ram68k_ack	=> ram68k_ack,
	ram68k_we	=> ram68k_we,
	ram68k_a		=> ram68k_a,
	ram68k_d		=> ram68k_d,
	ram68k_q		=> ram68k_q,
	ram68k_u_n	=> ram68k_u_n,
	ram68k_l_n	=> ram68k_l_n,

	vram_req	=> vram_req,
	vram_ack => vram_ack,
	vram_we	=> vram_we,
	vram_a	=> vram_a,
	vram_d	=> vram_d,
	vram_q	=> vram_q,
	vram_u_n => vram_u_n,
	vram_l_n => vram_l_n,
	
	initDone 	=> SDR_INIT_DONE
);

gen : entity work.gen_top
	port map(
		MRST_N		=> PRE_RESET_N,
		TG68_RES_N	=> PRE_RESET_N and host_bootdone,
		MCLK			=> MCLK,
		
		-- "FLASH"
		romrd_req	=> romrd_req,
		romrd_ack	=> romrd_ack,
		romrd_a		=> romrd_a,
		romrd_q		=> romrd_q,

		-- 68000 RAM
		ram68k_req	=> ram68k_req,
		ram68k_ack	=> ram68k_ack,
		ram68k_we	=> ram68k_we,
		ram68k_a		=> ram68k_a,
		ram68k_d 	=> ram68k_d,
		ram68k_q		=> ram68k_q,
		ram68k_l_n	=> ram68k_l_n,
		ram68k_u_n	=> ram68k_u_n,

		-- VRAM
		vram_req		=> vram_req,
		vram_ack		=> vram_ack,
		vram_we		=> vram_we,
		vram_a		=> vram_a,
		vram_d		=> vram_d,
		vram_q		=> vram_q,
		vram_l_n		=> vram_l_n,
		vram_u_n		=> vram_u_n,

		-- Video Output
		RED			=> VDP_RED,
		GREEN			=> VDP_GREEN,
		BLUE			=> VDP_BLUE,
		VS_N			=> VDP_VS_N,
		HS_N			=> VDP_HS_N,

		VGA_RED		=> VDP_VGA_RED,
		VGA_GREEN	=> VDP_VGA_GREEN,
		VGA_BLUE		=> VDP_VGA_BLUE,
		VGA_VS_N		=> VDP_VGA_VS_N,
		VGA_HS_N		=> VDP_VGA_HS_N,

		-- Audio
		MASTER_VOLUME	=> MASTER_VOLUME,
		DAC_LDATA => audiol,
		DAC_RDATA => audior,

		-- Joystick ports (Port_A, Port_B)
		JOY_1 => joya_swap,
		JOY_2 => joyb_swap,
		SW => SW
);


-- UART_TX <='1';

mist_console_d: component mist_console
	generic map
	( CLKFREQ => 108)
	port map
	(
		clk => memclk,
		n_reset => reset,
		ser_in => txd,
		par_out_data => par_out_data,
		par_out_strobe => par_out_strobe
	);

sd_card_d: component sd_card
	port map
	(
 		-- connection to io controller
 		io_lba => sd_lba,
 		io_rd  => sd_rd,
		io_wr  => sd_wr,
 		io_ack => sd_ack,
		io_conf => sd_conf,
		io_sdhc => sd_sdhc,
 		io_din => sd_data_in,
 		io_din_strobe => sd_data_in_strobe,
		io_dout => sd_data_out,
		io_dout_strobe => sd_data_out_strobe,
 
		allow_sdhc  => '1',
		
 		-- connection to host
 		sd_cs  => spi_cs,
 		sd_sck => spi_clk,
		sd_sdi => spi_mosi,
		sd_sdo => spi_miso		
	);
	
user_io_d : user_io
    generic map (STRLEN => 1)
    port map (
      SPI_CLK => spirecoveredclock,
      SPI_SS_IO => CONF_DATA0,
      SPI_MISO => SPI_DO,
      SPI_MOSI => SPI_DI,
      conf_str => "00000000",   -- no config string -> no osd
      status => status,
		
 		-- connection to io controller
		sd_lba  => sd_lba,
		sd_rd   => sd_rd,
		sd_wr   => sd_wr,
		sd_ack  => sd_ack,
		sd_sdhc => sd_sdhc,
		sd_conf => sd_conf,
 		sd_dout => sd_data_in,
 		sd_dout_strobe => sd_data_in_strobe,
		sd_din => sd_data_out,
		sd_din_strobe => sd_data_out_strobe,

      joystick_0 => joy_0,
      joystick_1 => joy_1,
      joystick_2 => joy_2,
      joystick_3 => joy_3,
      joystick_4 => joy_4,
      joystick_analog_0 => joy_ana_0,
      joystick_analog_1 => joy_ana_1,
--      switches => switches,
       BUTTONS => buttons,
		ps2_clk => ps2_clk,
      ps2_kbd_clk => ps2k_clk_in,
      ps2_kbd_data => ps2k_dat_in,
      ps2_mouse_clk => ps2m_clk_in,
      ps2_mouse_data => ps2m_dat_in,
 		serial_data => par_out_data,
 		serial_strobe => par_out_strobe
 );

vga_window<='1';
mydither : component video_vga_dither
	generic map (
		outbits => 6
	)
	port map (
		clk => memclk,
		hsync => vga_hsync_i,
		vsync => vga_vsync_i,
		vid_ena => vga_window,
		iRed => unsigned(vga_red_o),
		iGreen => unsigned(vga_green_o),
		iBlue => unsigned(vga_blue_o),
		std_logic_vector(oRed) => VGA_R,
		std_logic_vector(oGreen) => VGA_G,
		std_logic_vector(oBlue) => VGA_B
	);
 
 -- If 15kHz Video - composite sync to VGA_HS and VGA_VS high for MiST RGB cable
VGA_HS <= not (vga_hsync_i xor vga_vsync_i) when SW(0)='1' else vga_hsync_i;
VGA_VS <= '1' when SW(0)='1' else vga_vsync_i;

-- Do we have audio?  If so, instantiate a two DAC channels.
leftsd: component hybrid_pwm_sd
	port map
	(
		clk => MCLK,
		n_reset => reset,
		din => not audiol(15) & std_logic_vector(audiol(14 downto 0)),
		dout => AUDIO_L
	);
	
rightsd: component hybrid_pwm_sd
	port map
	(
		clk => MCLK,
		n_reset => reset,
		din => not audior(15) & std_logic_vector(audior(14 downto 0)),
		dout => AUDIO_R
	);

-- #############################################################################
-- #############################################################################
-- #############################################################################

-- Boot process

FL_DQ<=boot_data;

process( memclk )
begin
	if rising_edge( memclk ) then
		if PRE_RESET_N = '0' then
				
			boot_req <='0';
			
			romwr_req <= '0';
			romwr_a <= to_unsigned(0, 21);
			bootState<=BOOT_READ_1;
			
		else
			case bootState is 
				when BOOT_READ_1 =>
					boot_req<='1';
					if boot_ack='1' then
						boot_req<='0';
						bootState <= BOOT_WRITE_1;
					end if;
					if host_bootdone='1' then
						boot_req<='0';
						bootState <= BOOT_DONE;
					end if;
				when BOOT_WRITE_1 =>
					romwr_d <= FL_DQ;
					romwr_req <= not romwr_req;
					bootState <= BOOT_WRITE_2;
				when BOOT_WRITE_2 =>
					if romwr_req = romwr_ack then
						romwr_a <= romwr_a + 1;
						bootState <= BOOT_READ_1;
					end if;
				when others => null;
			end case;	
		end if;
	end if;
end process;


-- Control module:

mycontrolmodule : entity work.CtrlModule
	generic map (
		sysclk_frequency => 1080 -- Sysclk frequency * 10
	)
	port map (
		clk => memclk,
		reset_n => reset,

		-- SPI signals
		spi_miso	=> spi_miso,
		spi_mosi => spi_mosi,
		spi_clk => spi_clk,
		spi_cs => spi_cs,
		
		-- UART
		rxd => UART_RX,
		txd => UART_TX,
		
		-- DIP switches
		dipswitches => SW,

		-- PS2 keyboard
		ps2k_clk_in => ps2k_clk_in,
		ps2k_dat_in => ps2k_dat_in,
		ps2k_clk_out => ps2k_clk_out,
		ps2k_dat_out => ps2k_dat_out,
		
		-- Host control
		host_reset_n => host_reset_n,
		host_bootdone => host_bootdone,
		
		-- Host boot data
		host_bootdata => boot_data,
		host_bootdata_req => boot_req,
		host_bootdata_ack => boot_ack,
		rommap => rommap,
		
		-- Video signals for OSD
		vga_hsync => vga_hsync_i,
		vga_vsync => vga_vsync_i,
		osd_window => osd_window,
		osd_pixel => osd_pixel,
		
		vol_master => MASTER_VOLUME,
		
		-- Gamepad emulation
		gp1emu => gp1emu,
		gp2emu => gp2emu
);


overlay : entity work.OSD_Overlay
	port map
	(
		clk => memclk,
		red_in => vga_red_i,
		green_in => vga_green_i,
		blue_in => vga_blue_i,
		window_in => '1',
		osd_window_in => osd_window,
		osd_pixel_in => osd_pixel,
		hsync_in => vga_hsync_i,
		red_out => vga_red_o,
		green_out => vga_green_o,
		blue_out => vga_blue_o,
		window_out => open,
		scanline_ena => SW(1)
	);

-- Reset
PRE_RESET_N <= reset and SDR_INIT_DONE and host_reset_n;

-- Route VDP signals to outputs
RED <= VDP_RED & VDP_RED;
GREEN <= VDP_GREEN & VDP_GREEN;
BLUE <= VDP_BLUE & VDP_BLUE;
HS_N <= VDP_HS_N;
VS_N <= VDP_VS_N;

VGA_RED <= VDP_VGA_RED & VDP_VGA_RED;
VGA_GREEN <= VDP_VGA_GREEN & VDP_VGA_GREEN;
VGA_BLUE <= VDP_VGA_BLUE & VDP_VGA_BLUE;
VGA_HS_N <= VDP_VGA_HS_N;
VGA_VS_N <= VDP_VGA_VS_N;

-- Select between VGA and TV output	
vga_red_i <= RED when SW(0)='1' else VGA_RED;
vga_green_i <= GREEN when SW(0)='1' else VGA_GREEN;
vga_blue_i <= BLUE when SW(0)='1' else VGA_BLUE;
vga_hsync_i <= HS_N when SW(0)='1' else VGA_HS_N;
vga_vsync_i <= VS_N when SW(0)='1' else VGA_VS_N;

-- Map joystick buttons
joya(0) <= not joy_0(3) and gp1emu(0);
joya(1) <= not joy_0(2) and gp1emu(1);
joya(2) <= not joy_0(1) and gp1emu(2);
joya(3) <= not joy_0(0) and gp1emu(3);
joya(4) <= not joy_0(4) and gp1emu(4);
joya(5) <= not joy_0(5) and gp1emu(5);
joya(6) <= not joy_0(6) and gp1emu(6);
joya(7) <= not joy_0(7) and gp1emu(7);

joyb(0) <= not joy_1(3) and gp2emu(0);
joyb(1) <= not joy_1(2) and gp2emu(1);
joyb(2) <= not joy_1(1) and gp2emu(2);
joyb(3) <= not joy_1(0) and gp2emu(3);
joyb(4) <= not joy_1(4) and gp2emu(4);
joyb(5) <= not joy_1(5) and gp2emu(5);
joyb(6) <= not joy_1(6) and gp2emu(6);
joyb(7) <= not joy_1(7) and gp2emu(7);

-- Swap joysticks
joya_swap <= joya when SW(2)='1' else joyb;
joyb_swap <= joyb when SW(2)='1' else joya;

-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################

end architecture;
