library STD;
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity rom_loader is
	port(
		reset_n			: in std_logic;
		clk				: in std_logic;

		romwr_req		: out std_logic;
		romwr_ack		: in std_logic;
		romwr_we 		: out std_logic;
		romwr_a			: out std_logic_vector(21 downto 1);
		romwr_d			: out std_logic_vector(15 downto 0);

		boot_req			: out std_logic;
		boot_ack			: in std_logic;
		host_bootdone	: in std_logic
	);
end entity;

architecture rtl of rom_loader is

type bootStates is (BOOT_READ_1, BOOT_WRITE_1, BOOT_WRITE_2, BOOT_DONE);
signal bootState		: bootStates := BOOT_READ_1;

signal romwr_req_reg	: std_logic;
signal romwr_a_reg	: unsigned(21 downto 1);
signal boot_data		: std_logic_vector(15 downto 0);
signal FL_DQ			: std_logic_vector(15 downto 0);

begin

-- Assignments
romwr_req <= romwr_req_reg;
romwr_a <= std_logic_vector(romwr_a_reg);
FL_DQ <= boot_data;

-- Load process
process( clk )
begin
	if rising_edge( clk ) then
		if reset_n = '0' then
				
			boot_req <='0';
			
			romwr_req_reg <= '0';
			romwr_a_reg <= to_unsigned(0, 21);
			bootState <= BOOT_READ_1;
			
		else
			case bootState is 
				when BOOT_READ_1 =>
					boot_req <= '1';
					if boot_ack = '1' then
						boot_req <= '0';
						bootState <= BOOT_WRITE_1;
					end if;
					if host_bootdone = '1' then
						boot_req <= '0';
						bootState <= BOOT_DONE;
					end if;
				when BOOT_WRITE_1 =>
					romwr_d <= FL_DQ;
					romwr_req_reg <= not romwr_req_reg;
					bootState <= BOOT_WRITE_2;
				when BOOT_WRITE_2 =>
					if romwr_req_reg = romwr_ack then
						romwr_a_reg <= romwr_a_reg + 1;
						bootState <= BOOT_READ_1;
					end if;
				when others => null;
			end case;	
		end if;
	end if;
end process;

end architecture;