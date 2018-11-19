library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

-- -----------------------------------------------------------------------

entity sdram_controller is
	generic (
		colAddrBits : integer := 9;
		rowAddrBits : integer := 13;
		rasCasTiming : integer := 2;
		prechargeTiming: integer := 2
	);
	port (
		-- System
		clk : in std_logic;
		reset_n : in std_logic :='1';

		-- SDRAM interface
		sd_data : inout unsigned(15 downto 0);
		sd_addr : out unsigned(rowAddrBits-1 downto 0);
		sd_we_n : out std_logic;
		sd_ras_n : out std_logic;
		sd_cas_n : out std_logic;
		sd_ba_0 : out std_logic;
		sd_ba_1 : out std_logic;
		sd_ldqm : out std_logic;
		sd_udqm : out std_logic;

		ram68k_req : in std_logic;
		ram68k_ack : out std_logic;
		ram68k_we : in std_logic;
		ram68k_a : in std_logic_vector(15 downto 1);
		ram68k_d : in std_logic_vector(15 downto 0);
		ram68k_q : out std_logic_vector(15 downto 0);
		ram68k_u_n : in std_logic;
		ram68k_l_n : in std_logic;

		sram_req : in std_logic;
		sram_ack : out std_logic;
		sram_we : in std_logic;
		sram_a : in std_logic_vector(15 downto 1);
		sram_d : in std_logic_vector(15 downto 0);
		sram_q : out std_logic_vector(15 downto 0);
		sram_u_n : in std_logic;
		sram_l_n : in std_logic;

		vram_req : in std_logic;
		vram_ack : out std_logic;
		vram_we : in std_logic;
		vram_a : in std_logic_vector(15 downto 1);
		vram_d : in std_logic_vector(15 downto 0);
		vram_q : out std_logic_vector(15 downto 0);
		vram_u_n : in std_logic;
		vram_l_n : in std_logic;
		
		romwr_req : in std_logic;
		romwr_ack : out std_logic;
		romwr_we : in std_logic;
		romwr_a : in std_logic_vector(23 downto 1);
		romwr_d : in std_logic_vector(15 downto 0);
		romwr_q : out std_logic_vector(15 downto 0);
		
		romrd_req : in std_logic;
		romrd_ack : out std_logic;
		romrd_a : in std_logic_vector(23 downto 3);
		romrd_q : out std_logic_vector(63 downto 0);

		
--GE Temporary
		initDone : out std_logic
	);
end entity;

-- -----------------------------------------------------------------------

architecture rtl of sdram_controller is

	constant addrwidth : integer := rowAddrBits+colAddrBits+2;

	signal romrd_a_u : unsigned(addrwidth downto 3);
	signal romrd_q_u : unsigned(63 downto 0);
	
	signal romwr_a_u : unsigned(addrwidth downto 1);
	signal romwr_d_u : unsigned(15 downto 0);
	signal romwr_q_u : unsigned(15 downto 0);

	signal ram68k_a_u : unsigned(addrwidth downto 1);
	signal ram68k_d_u : unsigned(15 downto 0);
	signal ram68k_q_u : unsigned(15 downto 0);

	signal sram_a_u : unsigned(addrwidth downto 1);
	signal sram_d_u : unsigned(15 downto 0);
	signal sram_q_u : unsigned(15 downto 0);

	signal vram_a_u : unsigned(addrwidth downto 1);
	signal vram_d_u : unsigned(15 downto 0);
	signal vram_q_u : unsigned(15 downto 0);

	
begin
	
	romrd_a_u <= unsigned(std_logic_vector(to_unsigned(0, addrwidth - 23)) & romrd_a);
	romrd_q <= std_logic_vector(romrd_q_u);
	
	romwr_a_u <= unsigned(std_logic_vector(to_unsigned(0, addrwidth - 23)) & romwr_a);
	romwr_d_u <= unsigned(romwr_d);
	romwr_q <= std_logic_vector(romwr_q_u);

	ram68k_a_u <= unsigned(std_logic_vector(to_unsigned(2#100000000#, addrwidth - 15)) & ram68k_a);
	ram68k_d_u <= unsigned(ram68k_d);
	ram68k_q <= std_logic_vector(ram68k_q_u);

	sram_a_u <= unsigned(std_logic_vector(to_unsigned(2#010000000#, addrwidth - 15)) & sram_a);
	sram_d_u <= unsigned(sram_d);
	sram_q <= std_logic_vector(sram_q_u);

	vram_a_u <= unsigned(std_logic_vector(to_unsigned(2#110000000#, addrwidth - 15)) & vram_a);
	vram_d_u <= unsigned(vram_d);
	vram_q <= std_logic_vector(vram_q_u);
	
-- -----------------------------------------------------------------------
-- SDRAM Controller
-- -----------------------------------------------------------------------
	sdr : entity work.chameleon_sdram
		generic map (
			casLatency => 2,
			colAddrBits => colAddrBits,
			rowAddrBits => rowAddrBits,
			prechargeTiming => prechargeTiming,
			rasCasTiming => rasCasTiming,
			t_ck_ns => 9.3
		)
		port map (
			clk => clk,
			reset_n => reset_n,

			reserve => '0',

			sd_data => sd_data,
			sd_addr => sd_addr,
			sd_we_n => sd_we_n,
			sd_ras_n => sd_ras_n,
			sd_cas_n => sd_cas_n,
			sd_ba_0 => sd_ba_0,
			sd_ba_1 => sd_ba_1,
			sd_ldqm => sd_ldqm,
			sd_udqm => sd_udqm,
			
			romwr_req => romwr_req,
			romwr_ack => romwr_ack,
			romwr_we => romwr_we,
			romwr_a => romwr_a_u,
			romwr_d => romwr_d_u,
			romwr_q => romwr_q_u,
			
			romrd_req => romrd_req,
			romrd_ack => romrd_ack,
			romrd_a => romrd_a_u,
			romrd_q => romrd_q_u,
	
			ram68k_req => ram68k_req,
			ram68k_ack => ram68k_ack,
			ram68k_we => ram68k_we,
			ram68k_a => ram68k_a_u,
			ram68k_d => ram68k_d_u,
			ram68k_q => ram68k_q_u,
			ram68k_u_n => ram68k_u_n,
			ram68k_l_n => ram68k_l_n,

			sram_req => sram_req,
			sram_ack => sram_ack,
			sram_we => sram_we,
			sram_a => sram_a_u,
			sram_d => sram_d_u,
			sram_q => sram_q_u,
			sram_u_n => sram_u_n,
			sram_l_n => sram_l_n,

			vram_req => vram_req,
			vram_ack => vram_ack,
			vram_we => vram_we,
			vram_a => vram_a_u,
			vram_d => vram_d_u,
			vram_q => vram_q_u,
			vram_u_n => vram_u_n,
			vram_l_n => vram_l_n,
			
			initDone => initDone,
			
			debugIdle => open,
			debugRefresh => open
		);

end architecture;
