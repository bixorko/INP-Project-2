-- cpu.vhd: Simple 8-bit CPU (BrainF*ck interpreter)
-- Copyright (C) 2019 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Peter Vinarcik 
-- Login :    xvinar00
-- Date :     20.12.2019

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

   signal pc_reg : std_logic_vector(12 downto 0);
	signal pc_inc : std_logic;
	signal pc_dec : std_logic;
	 
	signal ptr_reg : std_logic_vector(12 downto 0);
	signal ptr_inc : std_logic;
	signal ptr_dec : std_logic;

	type instructions is (PtrIncs, PtrDec, ValInc, ValDec, PrintValue, Halt, WhileStart, WhileEnd, SaveToTmp, LoadFromTmp, DoNoth);
	signal ToDecodeInstr : instructions;
	
	type fsm_state is (idle, fetch, fetch2, FValInc, FValDec, FPrintValue, LetsDecode, FHalt, FWhileStart, FWhileStart2, FWhileEnd, FWhileEnd2, FWhileEnd3, FSaveToTmp, FLoadFromTmp);
	signal ActState : fsm_state;
	signal NextState : fsm_state;
	
	signal sel1 : std_logic;
	signal sel2 : std_logic;
	signal sel3 : std_logic_vector(1 downto 0);

begin
	pc_cntr: process (RESET, CLK)
	begin
		if (RESET='1') then
			pc_reg <= (others=>'0');
		elsif (CLK'event) and (CLK='1') then
			if (pc_inc = '1') then
				pc_reg <= pc_reg + 1;
			elsif(pc_dec = '1') then
				pc_reg <= pc_reg - 1;
			end if;
		end if;
	end process;

	ptr_cntr: process(RESET,CLK)
	begin
		if (RESET = '1') then
			ptr_reg <= "1000000000000";
		elsif (CLK'event) and (CLK='1') then
			if(ptr_inc = '1') then
				ptr_reg <= ptr_reg + 1;
			elsif(ptr_dec = '1') then
				ptr_reg <= ptr_reg - 1;
			end if;
		end if;
	end process;

	MUX12: process (sel1, sel2, pc_reg, ptr_reg) 
	begin
			  case sel1 is --MUX1
						 when '1' => DATA_ADDR <= pc_reg;
						 when '0' => 
							case sel2 is --MUX2
							   when '0' => DATA_ADDR <= ptr_reg;
								when '1' => DATA_ADDR <= "1000000000000";
								when others =>
							end case;
						 when others =>
			  end case;
	end process MUX12;

	MUX3: process (sel3, DATA_RDATA, IN_DATA)
	begin
			  case sel3 is --MUX3
			          when "00" => DATA_WDATA <= IN_DATA;
						 when "01" => DATA_WDATA <= DATA_RDATA + "00000001";
						 when "10" => DATA_WDATA <= DATA_RDATA - "00000001";
						 when "11" => DATA_WDATA <= DATA_RDATA;
						 when others =>
			  end case;
	end process MUX3;

	Load_Instruction : process (DATA_RDATA)
	begin
		case DATA_RDATA is
			when X"3E" => ToDecodeInstr <= PtrIncs;		-- >
			when X"3C" => ToDecodeInstr <= PtrDec; 	   -- <
			when X"2B" => ToDecodeInstr <= ValInc;		   -- +
			when X"2D" => ToDecodeInstr <= ValDec; 		-- - 
			when X"5B" => ToDecodeInstr <= WhileStart; 	-- [
			when X"5D" => ToDecodeInstr <= WhileEnd; 	   -- ]
			when X"2E" => ToDecodeInstr <= PrintValue;   -- .
			--when X"2C" => ToDecodeInstr <= LoadValue;  -- ,
			when X"24" => ToDecodeInstr <= SaveToTmp; 	-- $
			when X"21" => ToDecodeInstr <= LoadFromTmp;  -- !
			when X"00" => ToDecodeInstr <= Halt; 		   -- null
			when others => ToDecodeInstr <= DoNoth; 		   
		end case;
	end process;

	fsm_pstate : process (CLK, RESET, EN)
	begin
		if(RESET = '1') then
			ActState <= idle;
		elsif(CLK'event) and (CLK = '1') then
			if (EN = '1') then
				ActState <= NextState;
			end if;		
		end if;
	end process;
	
	fsm_nstate : process (EN, ActState, IN_VLD, IN_DATA, DATA_RDATA, OUT_BUSY, ToDecodeInstr, sel1, sel2, sel3)
	begin
	
	pc_inc <= '0';
	pc_dec <= '0';
	ptr_inc <= '0';
	ptr_dec <= '0';

	DATA_EN <= '0';
   	OUT_WE <= '0';
	DATA_RDWR <= '0';
	IN_REQ <= '0'; 
	
	case ActState is
		when idle =>
			NextState <= fetch;
		when LetsDecode =>
			case ToDecodeInstr is
				when PtrIncs => 
					ptr_inc <= '1';
					pc_inc <= '1';
					NextState <= fetch;
					
				when PtrDec =>
					ptr_dec <= '1';
					pc_inc <= '1';
					NextState <= fetch;
					
				when ValInc => 
					DATA_EN <= '1';
					DATA_RDWR <= '0';
					sel1 <= '0';
					sel2 <= '0';
					sel3 <= "01";
					NextState <= FValInc;
					
				when ValDec =>
					DATA_EN <= '1';
					DATA_RDWR <= '0';
					sel1 <= '0';
					NextState <= FValDec;
					
				when WhileStart =>
					pc_inc <= '1';
					DATA_EN <= '1';
					DATA_RDWR <= '0';
					sel1 <= '0';
					sel2 <= '0';
					if (DATA_RDATA = 0) then
						NextState <= FWhileStart;
					else
						NextState <= fetch;
					end if;
					
				when WhileEnd =>
					sel1 <= '0';
					sel2 <= '0';
					DATA_RDWR <= '0';
					DATA_EN <= '1';
					NextState <= FWhileEnd;
					
				when PrintValue => 
					DATA_EN <= '1';
					DATA_RDWR <= '0';
					sel1 <= '0';
					sel2 <= '0';
					NextState <= FPrintValue;
					
				--when LoadValue => 
				
				when SaveToTmp => 
					DATA_EN <= '1';
					DATA_RDWR <= '0';
					sel1 <= '0';	
					sel2 <= '0';
					NextState <= FSaveToTmp;
					
				when LoadFromTmp => 
					DATA_EN <= '1';
					DATA_RDWR <= '0';
					sel1 <= '0';	
					sel2 <= '1';
					NextState <= FLoadFromTmp;
					
				when Halt => 
					NextState <= FHalt;
				
				when DoNoth => 
				   NextState <= fetch;
				when others => 
			end case;
			
		when FSaveToTmp =>
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			sel1 <= '0';
			sel2 <= '1';
			sel3 <= "11";
			pc_inc <= '1';
			NextState <= fetch;
		
		when FLoadFromTmp =>
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			sel1 <= '0';
			sel2 <= '0';
			sel3 <= "11";
			pc_inc <= '1';
			NextState <= fetch;
			
		when FValInc =>
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			sel1 <= '0';
			sel2 <= '0';
			sel3 <= "01";
			pc_inc <= '1';
			NextState <= fetch;
			
		when FValDec => 
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			sel1 <= '0';
			sel2 <= '0';
			sel3 <= "10";
			pc_inc <= '1';
			NextState <= fetch;
			
		when FWhileStart =>
			sel1 <= '1';
			DATA_EN <= '1';
			NextState <= FWhileStart2;
			
		when FWhileStart2 =>
			pc_inc <= '1';
			if (ToDecodeInstr = WhileEnd) then			
				NextState <= fetch;
			else
				NextState <= FWhileStart;
			end if;
			
		when FWhileEnd =>
			if (DATA_RDATA = 0) then
				pc_inc <= '1';
				NextState <= fetch;
			else
				pc_dec <= '1';
				NextState <= FWhileEnd2;
			end if;
			
		when FWhileEnd2 =>
			sel1 <= '1';
			DATA_EN <= '1';
			NextState <= FWhileEnd3;
			
		when FWhileEnd3 =>
			if (ToDecodeInstr = WhileStart) then
				NextState <= fetch;
			else
				pc_dec <= '1';
				NextState <= FWhileEnd2;
			end if;
			
		when fetch =>
			NextState <= fetch2;
			
		when fetch2 =>
			DATA_EN <= '1';
			sel1 <= '1';
			NextState <= LetsDecode;
			
		when FPrintValue =>
			if (OUT_BUSY = '0') then
				OUT_WE <= '1';
				OUT_DATA <= DATA_RDATA;
				pc_inc <= '1';
				NextState <= fetch;
			else 
				NextState <= FPrintValue;
			end if;
		when FHalt =>
			NextState <= FHalt;
	end case;
	end process;

end behavioral;
 
