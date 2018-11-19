-- -----------------------------------------------------------------------
--
-- Turbo Chameleon
--
-- Multi purpose FPGA expansion for the Commodore 64 computer
--
-- -----------------------------------------------------------------------
-- Copyright 2005-2011 by Peter Wendrich (pwsoft@syntiac.com)
-- All Rights Reserved.
--
-- Your allowed to re-use this file for non-commercial applications
-- developed for the Turbo Chameleon 64 cartridge. Either open or closed
-- source whatever might be required by other licenses.
--
-- http://www.syntiac.com/chameleon.html
-- -----------------------------------------------------------------------
--
-- SDRAM controller
--
-- Changes by AMR
-- Removed unused ports
--
-- -----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

-- -----------------------------------------------------------------------

entity chameleon_sdram is
	generic (
		-- SDRAM cols/rows  8/12 = 8 Mbyte, 9/12 = 16 Mbyte, 9/13 = 32 Mbyte
		colAddrBits : integer := 9;
		rowAddrBits : integer := 12;
		rasCasTiming : integer := 2;
		prechargeTiming: integer := 2;

		-- Controller settings
		initTimeout : integer := 10000;
	-- SDRAM timing
		casLatency : integer := 3;
		t_refresh_ms  : real := 64.0;
		t_ck_ns  : real := 10.0 -- Clock cycle time
	);
	port (
-- System
		clk : in std_logic;
		reset_n : in std_logic := '1';

		reserve : in std_logic := '0';
		delay_refresh : in std_logic := '0';

-- SDRAM interface
		sd_data : inout unsigned(15 downto 0);
		sd_addr : out unsigned((rowAddrBits-1) downto 0);
		sd_we_n : out std_logic;
		sd_ras_n : out std_logic;
		sd_cas_n : out std_logic;
		sd_ba_0 : out std_logic;
		sd_ba_1 : out std_logic;
		sd_ldqm : out std_logic;
		sd_udqm : out std_logic;

--GE
		
		romwr_req : in std_logic;
		romwr_ack : out std_logic;
		romwr_we : in std_logic;
		romwr_a : in unsigned((colAddrBits+rowAddrBits+2) downto 1);
		romwr_d : in unsigned(15 downto 0);
		romwr_q : out unsigned(15 downto 0);
		
		romrd_req : in std_logic;
		romrd_ack : out std_logic;
		romrd_a : in unsigned((colAddrBits+rowAddrBits+2) downto 3);
		romrd_q : out unsigned(63 downto 0);

		ram68k_req : in std_logic;
		ram68k_ack : out std_logic;
		ram68k_we : in std_logic;
		ram68k_a : in unsigned((colAddrBits+rowAddrBits+2) downto 1);
		ram68k_d : in unsigned(15 downto 0);
		ram68k_q : out unsigned(15 downto 0);
		ram68k_u_n : in std_logic;
		ram68k_l_n : in std_logic;

		sram_req : in std_logic;
		sram_ack : out std_logic;
		sram_we : in std_logic;
		sram_a : in unsigned((colAddrBits+rowAddrBits+2) downto 1);
		sram_d : in unsigned(15 downto 0);
		sram_q : out unsigned(15 downto 0);
		sram_u_n : in std_logic;
		sram_l_n : in std_logic;

		vram_req : in std_logic;
		vram_ack : out std_logic;
		vram_we : in std_logic;
		vram_a : in unsigned((colAddrBits+rowAddrBits+2) downto 1);
		vram_d : in unsigned(15 downto 0);
		vram_q : out unsigned(15 downto 0);
		vram_u_n : in std_logic;
		vram_l_n : in std_logic;
		
--GE Temporary
		initDone : out std_logic;
		
-- Debug ports
		debugIdle : out std_logic;  -- '1' memory is idle
		debugRefresh : out std_logic -- '1' memory is being refreshed
	);
end entity;

-- -----------------------------------------------------------------------

architecture rtl of chameleon_sdram is
	constant refresh_interval : integer := integer((t_refresh_ms*1000000.0) / (t_ck_ns * 2.0**rowAddrBits));
-- ram state machine
	type ramStates is (
		RAM_INIT,
		RAM_INIT_PRECHARGE,
		RAM_INITAUTO1,
		RAM_INITAUTO2,
		RAM_SETMODE,
		RAM_IDLE,
		
		RAM_ACTIVE,
		RAM_READ_1,
		RAM_READ_TERMINATEBURST,
		RAM_READ_BURST,
		RAM_READ_2,
		RAM_READ_3,
		RAM_READ_4,
		RAM_READ_5,
		RAM_WRITE_1,
		RAM_WRITE_2,
		RAM_WRITE_3,
		RAM_WRITE_4,
		RAM_WRITE_ABORT,
		RAM_WRITE_DLY,
		
		RAM_PRECHARGE,
		RAM_PRECHARGE_ALL,
		RAM_AUTOREFRESH
	);
	
	type ramPorts is (
		PORT_NONE,
		PORT_ROMRD,
		PORT_ROMWR,
		PORT_RAM68K,
		PORT_VRAM,
		PORT_SRAM
	);
	
	subtype row_t is unsigned((rowAddrBits-1) downto 0);
	subtype col_t is unsigned((colAddrBits-1) downto 0);
	
	signal ramTimer : integer range 0 to 32767;
	signal ramState : ramStates := RAM_INIT;
	signal ramAlmostDone : std_logic;  -- FIXME redundant
	signal ramDone : std_logic;
	
	signal ram_data_reg : unsigned(sd_data'range);

	signal cache_ack_reg : std_logic := '0';
	signal cache_write : std_logic := '0';
	signal cache_writeack : std_logic := '0';

-- Registered sdram signals
	signal sd_data_reg : unsigned(15 downto 0);
	signal sd_data_ena : std_logic := '0';
	signal sd_addr_reg : unsigned((rowAddrBits-1) downto 0);
	signal sd_we_n_reg : std_logic;
	signal sd_ras_n_reg : std_logic;
	signal sd_cas_n_reg : std_logic;
	signal sd_ba_0_reg : std_logic;
	signal sd_ba_1_reg : std_logic;
	signal sd_ldqm_reg : std_logic;
	signal sd_udqm_reg : std_logic;

--GE
	signal romwr_ackReg : std_logic := '0';
	signal romrd_ackReg : std_logic := '0';
	signal ram68k_ackReg : std_logic := '0';
	signal vram_ackReg : std_logic := '0';
	signal sram_ackReg : std_logic := '0';
	
--GE
	signal romwr_qReg : unsigned(15 downto 0);	
	signal romrd_qReg : unsigned(63 downto 0);
	signal ram68k_qReg : unsigned(15 downto 0);	
	signal vram_qReg : unsigned(15 downto 0);	
	signal sram_qReg : unsigned(15 downto 0);
	
	signal initDoneReg : std_logic := '0';
	
-- RAM buffers
	signal vicvid_buffer : unsigned(63 downto 0);
	
-- Active rows in SDRAM
--	type bankRowDef is array(0 to 3) of row_t;
--	signal bankActive : std_logic_vector(0 to 3) := (others => '0');
--	signal bankRow : bankRowDef;

-- Memory auto refresh
	constant refreshClocks : integer := 9;
	signal refreshTimer : integer range 0 to 2047 := 0;
	signal refreshActive : std_logic := '0';
	signal refreshSubtract : std_logic := '0';

	signal currentState : ramStates;
	signal currentPort : ramPorts;
	signal currentBank : unsigned(1 downto 0);
	signal currentRow : row_t;
	signal currentCol : col_t;
	signal currentRdData : unsigned(63 downto 0);
	signal currentWrData : unsigned(63 downto 0);
	signal currentLdqm : std_logic;
	signal currentUdqm : std_logic;
	signal currentBurst : std_logic;

	signal preselectBank : std_logic;
	signal preselectBankPause : integer range 0 to 4;
	
	signal nextRamBank : unsigned(1 downto 0);
	signal nextRamRow : row_t;
	signal nextRamCol : col_t;
	signal nextRamPort : ramPorts;
	signal nextRamState : ramStates;
	signal nextLdqm : std_logic;
	signal nextUdqm : std_logic;
	signal nextBurst : std_logic;
	
	type ramPort_record is record
		ramport : ramPorts;
		bank : unsigned(1 downto 0);
		row : row_t;
		col : col_t;
		udqm : std_logic;
		ldqm : std_logic;
		pending : std_logic;
		burst : std_logic;
		wr : std_logic;
		fill : std_logic;
	end record;
	type ramPort_records is array(4 downto 0) of ramPort_record;
	signal ramPort_rec : ramPort_records;
	
	signal ramPort_pri : integer range 0 to 4;
	signal ramPort_req : std_logic;


	type bank_record is record
		row : row_t;
		rowopen : std_logic;
	end record;
	type bank_records is array(3 downto 0) of bank_record;
	signal banks : bank_records;

	subtype addr_bankbits is Natural range colAddrBits+2 downto colAddrBits+1;
	subtype addr_rowbits is Natural range colAddrBits+rowAddrBits+2 downto colAddrBits+3;
	subtype addr_colbits is Natural range colAddrBits downto 1;
	subtype addr_colbits_64bit is Natural range colAddrBits downto 3;

	COMPONENT TwoWayCache
	PORT
	(
		clk		:	 IN STD_LOGIC;
		reset_n	: IN std_logic;
		ready : out std_logic;
		cpu_addr		:	 IN UNSIGNED(31 DOWNTO 0);
		cpu_req		:	 IN STD_LOGIC;
		cpu_ack		:	 OUT STD_LOGIC;
		cpu_cachevalid		:	 OUT STD_LOGIC;
		cpu_rw_n		:	 IN STD_LOGIC;
		cpu_rwl_n	: in std_logic;
		cpu_rwu_n : in std_logic;
		data_from_cpu		:	 IN UNSIGNED(15 DOWNTO 0);
		data_to_cpu		:	 OUT UNSIGNED(15 DOWNTO 0);
		data_from_sdram		:	 IN UNSIGNED(15 DOWNTO 0);
		sdram_req		:	 OUT STD_LOGIC;
		sdram_fill		:	 IN STD_LOGIC
	);
END COMPONENT;

	
constant useCache : boolean := false;

-- Cache signals

signal cache_ready : std_logic;
signal cache_req : std_logic;
signal cache_req_d : std_logic_vector(1 downto 0);
signal cache_ack : std_logic;
signal cache_valid : std_logic;
signal cache_wr : std_logic;
signal cache_wrl : std_logic;
signal cache_wrh : std_logic;
signal cache_out : std_logic_vector(15 downto 0);
signal cache_req_out : std_logic;
signal cache_fill : std_logic;
begin

mytwc : component TwoWayCache
	PORT map
	(
		clk => clk,
		reset_n => reset_n,
		ready => cache_ready,
		cpu_addr(31 downto colAddrBits+rowAddrBits+3) => (others => '0'),
		cpu_addr(colAddrBits+rowAddrBits+2 downto 0) => vram_a&'0',
		cpu_req => cache_req,
		cpu_ack => cache_ack,
		cpu_cachevalid => cache_valid,
		cpu_rw_n => not vram_we,
		cpu_rwl_n => vram_l_n,
		cpu_rwu_n => vram_u_n,
		data_from_cpu => vram_d,
		data_to_cpu => vram_q,
		data_from_sdram => ram_data_reg,
		sdram_req => cache_req_out,
		sdram_fill => cache_fill
	);


-- -----------------------------------------------------------------------

	ram_data_reg <= sd_data;

-- -----------------------------------------------------------------------
-- Refresh timer
	process(clk)
	begin
		if rising_edge(clk) then
			if refreshSubtract = '1' then
				refreshTimer <= refreshTimer - refresh_interval;
			else
					refreshTimer <= refreshTimer + 1;
			end if;
		end if;
	end process;

	
-- -----------------------------------------------------------------------
-- Create row, column, bank and pending signals for each port

		-- ROM Write port

		ramPort_rec(0).pending<='1' when (romwr_req /= romwr_ackReg) and (currentPort /= PORT_ROMWR) else '0';
		ramPort_rec(0).bank<=romwr_a(addr_bankbits);
		ramPort_rec(0).row<=romwr_a(addr_rowbits);
		ramPort_rec(0).col<=romwr_a(addr_colbits);
		ramPort_rec(0).wr<=romwr_we;
		ramPort_rec(0).burst<='0';
		ramPort_rec(0).ldqm<='0';
		ramPort_rec(0).udqm<='0';
		ramPort_rec(0).ramport<=PORT_ROMWR;
		
		-- ROM Read port
		
		ramPort_rec(1).pending<='1' when (romrd_req /= romrd_ackReg) and (currentPort /= PORT_ROMRD) else '0';
		ramPort_rec(1).bank<=romrd_a(addr_bankbits);
		ramPort_rec(1).row<=romrd_a(addr_rowbits);
		ramPort_rec(1).col<=romrd_a(addr_colbits_64bit)&"00";
		ramPort_rec(1).wr<='0';
		ramPort_rec(1).burst<='1';
		ramPort_rec(1).ldqm<='0';
		ramPort_rec(1).udqm<='0';
		ramPort_rec(1).ramport<=PORT_ROMRD;

		
		-- 68K RAM port

		ramPort_rec(2).pending<='1' when (ram68k_req /= ram68k_ackReg) and (currentPort /= PORT_RAM68K) else '0';
		ramPort_rec(2).bank<=ram68k_a(addr_bankbits);
		ramPort_rec(2).row<=ram68k_a(addr_rowbits);
		ramPort_rec(2).col<=ram68k_a(addr_colbits);
		ramPort_rec(2).wr<=ram68k_we;
		ramPort_rec(2).burst<='0';
		ramPort_rec(2).ldqm<=ram68k_l_n;
		ramPort_rec(2).udqm<=ram68k_u_n;
		ramPort_rec(2).ramport<=PORT_RAM68K;

		
		-- VRAM port

		cache_req<='1' when (vram_req /= vram_ackReg) and 
				((currentPort /= PORT_VRAM) or cache_write='1') else '0';

		ramPort_rec(3).pending<='1' when cache_req_out='1' or
			((vram_req /= vram_ackReg) and (currentPort /= PORT_VRAM) and vram_we='1') else '0';
		ramPort_rec(3).bank<=vram_a(addr_bankbits);
		ramPort_rec(3).row<=vram_a(addr_rowbits);
		ramPort_rec(3).col<=vram_a(addr_colbits);
		ramPort_rec(3).wr<=vram_we;
		ramPort_rec(3).burst<='1';
		ramPort_rec(3).ldqm<=vram_l_n;
		ramPort_rec(3).udqm<=vram_u_n;
		ramPort_rec(3).ramport<=PORT_VRAM;
		cache_fill<=ramPort_rec(3).fill;


		-- SRAM port

		ramPort_rec(4).pending<='1' when (sram_req /= sram_ackReg) and (currentPort /= PORT_SRAM) else '0';
		ramPort_rec(4).bank<=sram_a(addr_bankbits);
		ramPort_rec(4).row<=sram_a(addr_rowbits);
		ramPort_rec(4).col<=sram_a(addr_colbits);
		ramPort_rec(4).wr<=sram_we;
		ramPort_rec(4).burst<='0';
		ramPort_rec(4).ldqm<=sram_l_n;
		ramPort_rec(4).udqm<=sram_u_n;
		ramPort_rec(4).ramport<=PORT_SRAM;
		
process(clk)
begin	
			if rising_edge(clk) then
				cache_req_d<=cache_req_d(0)&cache_req; -- Delay req signal to allow the cache address logic time to catch up.
			end if;
end process;
		
process(clk, ramPort_rec)
begin	
		-- Priority encode
		ramPort_pri<=0;
		ramPort_req<='0';
		for i in 0 to 4 loop
			if ramPort_rec(i).pending='1' then
				ramPort_pri<=i;
				ramPort_req<='1';
			end if;
		end loop;

end process;
	
-- -----------------------------------------------------------------------
-- State machine
	process(clk, ramPort_req, ramPort_rec, ramPort_pri)
	begin
			nextRamState <= RAM_IDLE;
			nextRamPort <= PORT_NONE;
			nextRamBank <= "00";
			nextRamRow <= ( others => '0');
			nextRamCol <= ( others => '0');
			nextLdqm <= '0';
			nextUdqm <= '0';
			nextBurst <= '0';

			if ramPort_req='1' then
				nextRamState <= RAM_READ_1;
				if ramPort_rec(ramPort_pri).wr = '1' then
					nextRamState <= RAM_WRITE_1;
					nextLdqm <= ramPort_rec(ramPort_pri).ldqm;
					nextUdqm <= ramPort_rec(ramPort_pri).udqm;
				end if;				
				nextBurst <= ramPort_rec(ramPort_pri).burst;
				nextRamPort <= ramPort_rec(ramPort_pri).ramport;
				nextRamBank <= ramPort_rec(ramPort_pri).bank;
				nextRamRow <= ramPort_rec(ramPort_pri).row;
				nextRamCol <= ramPort_rec(ramPort_pri).col;
			end if;

	end process;

	process(clk, sd_data_ena, sd_data_reg, sd_addr_reg,
			sd_ras_n_reg, sd_cas_n_reg, sd_we_n_reg,
			sd_ba_0_reg, sd_ba_1_reg, sd_ldqm_reg, sd_udqm_reg)
	begin
-- These are already registered by virtue of being assigned with a clock edge.
-- Registering them again gives the synthesis software the opportunity to pipeline
-- them if need be, but it shouldn't be necessary for the MD core.

--		if rising_edge(clk) then
			sd_data <= (others => 'Z');
			if sd_data_ena = '1' then
				sd_data <= sd_data_reg;
			end if;
			sd_addr <= sd_addr_reg;
			sd_ras_n <= sd_ras_n_reg;
			sd_cas_n <= sd_cas_n_reg;
			sd_we_n <= sd_we_n_reg;
			sd_ba_0 <= sd_ba_0_reg;
			sd_ba_1 <= sd_ba_1_reg;
			sd_ldqm <= sd_ldqm_reg;
			sd_udqm <= sd_udqm_reg;
--		end if;
	end process;

	process(clk)
	begin
		if rising_edge(clk) then
			refreshSubtract <= '0';
			ramAlmostDone <= '0';
			ramDone <= '0';
			sd_data_ena <= '0';
			sd_addr_reg <= (others => '0');
			sd_ras_n_reg <= '1';
			sd_cas_n_reg <= '1';
			sd_we_n_reg <= '1';
			
			sd_ba_0_reg <= '0';
			sd_ba_1_reg <= '0';

			sd_ldqm_reg <= '0';
			sd_udqm_reg <= '0';

			ramPort_rec(3).fill<='0';

			if ramTimer=0 then
				preselectBank<='0';
			end if;
			

			-- Issue the next bank's PRECHARGE and ACTIVE commands if we can.

			if preselectBankPause /= 0 then
				preselectBankPause <= preselectBankPause - 1;
			end if;
			
			if preselectBank='1' and preselectBankPause=0 then
				if nextRamState /= RAM_IDLE and
						(currentBank /= nextRamBank or ramAlmostDone='1') then
					-- Do we need to close a row first?
					if	banks(to_integer(nextRamBank)).rowopen='1' and
						banks(to_integer(nextRamBank)).row /= nextRamRow then
						-- Wrong row active in bank, do precharge to close the row
						sd_we_n_reg <= '0';
						sd_ras_n_reg <= '0';				
						sd_ba_0_reg <= nextRamBank(0);
						sd_ba_1_reg <= nextRamBank(1);
						banks(to_integer(nextRamBank)).rowopen <= '0';
						-- Ensure a gap of at least one clock between preselecion commands
						preselectBankPause<=prechargeTiming-1;
					elsif banks(to_integer(nextRamBank)).rowopen='0' then
						-- Open the next row
						sd_addr_reg <= nextRamRow;
						sd_ras_n_reg <= '0';
						sd_ba_0_reg <= nextRamBank(0);
						sd_ba_1_reg <= nextRamBank(1);
						banks(to_integer(nextRamBank)).row <= nextRamRow;
						banks(to_integer(nextRamBank)).rowopen <= '1';
						-- Ensure a gap of at least one clock between this and next command
						preselectBankPause<=rasCasTiming-1;
					end if;
				end if;
			end if;
			
			if ramTimer /= 0 then
				ramTimer <= ramTimer - 1;
			else
				case ramState is
				when RAM_INIT =>
					-- Wait for clock to stabilise and PLL locks
					-- Then follow init steps in datasheet:
					--   precharge all banks
					--   perform a few autorefresh cycles (we do 2 of them)
					--   setmode (burst and CAS latency)
					--   after a few clocks ram is ready for use (we wait 10 just to be sure).
					initDoneReg <= '0'; --GE
					ramTimer <= 20000;
					ramState <= RAM_INIT_PRECHARGE;
				when RAM_INIT_PRECHARGE =>
					-- Precharge all banks, part of initialisation sequence.
					ramTimer <= 100;
					ramState <= RAM_INITAUTO1;
					sd_ras_n_reg <= '0';
					sd_we_n_reg <= '0';
					sd_addr_reg(10) <= '1'; -- precharge all banks
				when RAM_INITAUTO1 =>
					-- refresh cycle to init ram (1st)
					ramTimer <= 10;
					ramState <= RAM_INITAUTO2;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';				
				when RAM_INITAUTO2 =>
					-- refresh cycle to init ram (2nd)
					ramTimer <= 10;
					ramState <= RAM_SETMODE;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';
				when RAM_SETMODE =>
					-- Set mode bits of RAM.
					ramTimer <= 10;
					ramState <= RAM_IDLE; -- ram is ready for commands after set-mode

					sd_addr_reg <= resize("001000100010", sd_addr'length); -- CAS2, Burstlength 4 (8 bytes, 64 bits), no burst on writes

					if casLatency = 3 then
						sd_addr_reg(6 downto 4) <= "011";
					end if;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';
					initDoneReg <= '1'; --GE
				when RAM_IDLE =>
					refreshActive <= '0';
					currentPort <= PORT_NONE;
					if preselectBankPause=0 then
					
						-- Do we have a request to service?
						if nextRamState /= RAM_IDLE then
							currentState <= nextRamState;
							currentPort <= nextRamPort;
							currentBank <= nextRamBank;
							currentRow <= nextRamRow;
							currentCol <= nextRamCol;
							currentLdqm <= nextLdqm;
							currentUdqm <= nextUdqm;
							currentBurst <= nextBurst;
							
							case nextRamPort is
								when PORT_ROMWR =>
									currentWrData(15 downto 0) <= romwr_d;
								when PORT_RAM68K =>
									currentWrData(15 downto 0) <= ram68k_d;
								when PORT_VRAM =>
									currentWrData(15 downto 0) <= vram_d;
								when PORT_SRAM =>
									currentWrData(15 downto 0) <= sram_d;
								when others =>
									null;
							end case;
							
							if nextRamState=RAM_WRITE_1 then	-- No need to wait for the write to complete before acknowledging
								if nextRamPort=PORT_VRAM then
									cache_write<='1';
								else
									ramDone<='1';
								end if;
							end if;

							ramState <= nextRamState;
							if banks(to_integer(nextRamBank)).rowopen = '0' then
								-- Current bank not active. Activate a row first
	--							ramState <= RAM_ACTIVE;
								-- Might as well open the row directly here.
								ramTimer <= rasCasTiming-1;
								sd_addr_reg <= nextRamRow;
								sd_ras_n_reg <= '0';
								sd_ba_0_reg <= nextRamBank(0);
								sd_ba_1_reg <= nextRamBank(1);
								banks(to_integer(nextRamBank)).row <= nextRamRow;
								banks(to_integer(nextRamBank)).rowopen <= '1';
							elsif banks(to_integer(nextRamBank)).row /= nextRamRow then
								-- Wrong row active in bank, do precharge then activate a row.
	--							ramState <= RAM_PRECHARGE;
								ramTimer <= prechargeTiming-1;
								ramState <= RAM_ACTIVE;
								sd_we_n_reg <= '0';
								sd_ras_n_reg <= '0';				
								sd_ba_0_reg <= nextRamBank(0);
								sd_ba_1_reg <= nextRamBank(1);
								banks(to_integer(nextRamBank)).rowopen <= '0';
							end if;

						elsif (delay_refresh = '0') and (refreshTimer > refresh_interval) then
							-- Refresh timeout, perform auto-refresh cycle
							refreshActive <= '1';
							refreshSubtract <= '1';
							ramSTate <= RAM_AUTOREFRESH;
							for i in 0 to 3 loop
								if banks(i).rowopen='1' then
									-- There are still rows active, so we precharge them first							
									ramState <= RAM_PRECHARGE_ALL;
								end if;
							end loop;

						end if; -- nextState/=RAM_IDLE
					end if; -- preselectBankPause

				when RAM_ACTIVE =>
					ramTimer <= 1;
					ramState <= currentState;
					sd_addr_reg <= currentRow;
					sd_ras_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);
					banks(to_integer(currentBank)).row <= currentRow;
					banks(to_integer(currentBank)).rowopen <= '1';
--					preselectBank<='1';
				when RAM_READ_1 =>
					if currentBurst='1' then
						if currentPort=PORT_VRAM then  -- FIXME - make this runtime selectable
							ramTimer <= casLatency-1;-- + 1;
							ramState <= RAM_READ_BURST;
							preselectBank<='1';
						else
							ramTimer <= casLatency;-- + 1;
							ramState <= RAM_READ_2;
							preselectBank<='1';
						end if;
					else
						ramState <= RAM_READ_TERMINATEBURST;
					end if;
					sd_addr_reg <= resize(currentCol, sd_addr'length);
					sd_cas_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);
--					preselectBank<='1';
				when RAM_READ_TERMINATEBURST =>
					preselectBank<='1';
					ramTimer <= casLatency-1;
					ramState <= RAM_READ_2;
					sd_we_n_reg <= '0';	-- Terminate Burst
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);
				when RAM_READ_BURST =>
						ramPort_rec(3).fill<='1';	-- FIXME - make the port number runtime selectable
						ramTimer<=3;
						ramState<=RAM_IDLE;			
				when RAM_READ_2 =>
					if currentBurst='1' then
						ramState <= RAM_READ_3;
--						preselectBank<='1';
					else
						ramDone <='1';
						ramState <= RAM_IDLE;
					end if;
					currentRdData(15 downto 0) <= ram_data_reg;
					case currentPort is
						when PORT_ROMWR => --GE
							romwr_qReg <= ram_data_reg;
						when PORT_RAM68K => --GE
							ram68k_qReg <= ram_data_reg;
						when PORT_VRAM => --GE
							vram_qReg <= ram_data_reg;
						when PORT_SRAM => --GE
							sram_qReg <= ram_data_reg;
						when others =>
							null;
					end case;
				when RAM_READ_3 =>
					-- FIXME - need to schdule this according to CAS Latency
					-- Terminate burst - assuming we're not in cache mode...
					sd_we_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);

					ramState <= RAM_READ_4;
					currentRdData(31 downto 16) <= ram_data_reg;
					ramAlmostDone <= '1'; -- Safe to issue next command to this bank
					preselectBank<='1';
				when RAM_READ_4 =>
					ramState <= RAM_READ_5;
					currentRdData(47 downto 32) <= ram_data_reg;
					ramAlmostDone <= '1'; -- Safe to issue next command to this bank
					preselectBank<='1';
				when RAM_READ_5 =>
					currentRdData(63 downto 48) <= ram_data_reg;
					ramState <= RAM_IDLE;
					ramDone <= '1';

				when RAM_WRITE_1 =>
					sd_data_ena <= '1';
					sd_we_n_reg <= '0';
					sd_cas_n_reg <= '0';
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);

					sd_addr_reg <= resize(currentCol, sd_addr'length);
					--GE sd_addr_reg <= resize(currentCol, sd_addr'length) or resize("10000000000", sd_addr'length); --GE Auto precharge

					sd_data_reg <= currentWrData(15 downto 0);
					sd_ldqm_reg <= currentLdqm;
					sd_udqm_reg <= currentUdqm;
					if cache_write='1' then
						ramState<=RAM_WRITE_2;
					else
						ramState<=RAM_IDLE;	-- FIXME - potentially violating write-to-precharge time here
					end if;
--					ramDone<='1';
				when RAM_WRITE_2 =>
					if cache_write='0' then
						ramState<=RAM_IDLE;	-- FIXME - potentially violating write-to-precharge time here
					end if;
				when RAM_PRECHARGE =>
					ramTimer <= 2;
					ramState <= RAM_ACTIVE;
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';				
					sd_ba_0_reg <= currentBank(0);
					sd_ba_1_reg <= currentBank(1);
					banks(to_integer(currentBank)).rowopen <= '0';
				when RAM_PRECHARGE_ALL =>
					ramTimer <= 2;
					ramState <= RAM_IDLE;
					if refreshActive = '1' then
						ramTimer <= 1;
						ramState <= RAM_AUTOREFRESH;
					end if;
					sd_addr_reg(10) <= '1'; -- All banks
					sd_we_n_reg <= '0';
					sd_ras_n_reg <= '0';
					for i in 0 to 3 loop
						banks(i).rowopen<='0';
					end loop;
				when RAM_AUTOREFRESH =>
					ramTimer <= refreshClocks;
					ramState <= RAM_IDLE;
					sd_we_n_reg <= '1';
					sd_ras_n_reg <= '0';
					sd_cas_n_reg <= '0';

				when others => null;
					
				end case;
			end if;
			
			if cache_ack='1' and vram_we='1' then
				cache_writeack<='1';
			end if;
			
			if cache_ack='1' or cache_writeack='1' then
				if cache_write='1' then
					ramDone<='1';
					cache_write<='0';
					cache_writeack<='0';
				end if;
			end if;
		end if;
	end process;

-- -----------------------------------------------------------------------
-- Debug and measurement signals
	debugIdle <= '1' when ((refreshActive = '0') and (ramState = RAM_IDLE)) else '0';
	debugRefresh <= refreshActive;

--GE -----------------------------------------------------------------------
	
	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_ROMWR
			and ramDone = '1' then
				romwr_ackReg <= not romwr_ackReg;
			end if;
		end if;
	end process;
	romwr_ack <= romwr_ackReg;
	romwr_q <= romwr_qReg; --GE

	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_ROMRD
			and ramDone = '1' then
				romrd_ackReg <= not romrd_ackReg;
				romrd_qReg <= currentRdData;
			end if;
		end if;
	end process;
	romrd_ack <= romrd_ackReg;
	romrd_q <= romrd_qReg; --GE


	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_RAM68K
			and ramDone = '1' then
				ram68k_ackReg <= not ram68k_ackReg;
			end if;
		end if;
	end process;
	ram68k_ack <= ram68k_ackReg;
	ram68k_q <= ram68k_qReg; --GE

	process(clk)
	begin
		if rising_edge(clk) then
			if currentPort = PORT_SRAM
			and ramDone = '1' then
				sram_ackReg <= not sram_ackReg;
			end if;
		end if;
	end process;
	sram_ack <= sram_ackReg;
	sram_q <= sram_qReg;

	process(clk)
	begin
		if rising_edge(clk) then
			if (currentPort = PORT_VRAM and 
							((vram_we='1' and ramDone = '1') or (vram_we='0' and cache_ack = '1')))
					or ((cache_req_d(0)='1' and cache_valid='1') and vram_we='0') then
				vram_ackReg <= vram_req;
			end if;
		end if;
	end process;
	vram_ack <= vram_ackReg;
	
--	vram_q <= vram_qReg; --GE

-- FIXME - have to make sure writes are acknowledged by the cache

	
	initDone <= initDoneReg; --Ge
	
end architecture;
