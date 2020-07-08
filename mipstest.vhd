----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 01/03/2020 09:47:13 PM
-- Design Name: 
-- Module Name: mipstest - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------
/*

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity dmem is
    port(clk, we: in std_logic;
         a, wd: in std_logic_vector(31 downto 0);
         rd : out std_logic_vector(31 downto 0));
end;
architecture behavioral of dmem is
begin
  process is
    type ramtype is array(63 downto 0) of std_logic_vector(31 downto 0);
    variable mem: ramtype;
  begin
    loop
      if(rising_edge(clk)) then
        if(we='1') then
          mem(to_integer(a(7 downto 2))) := wd;
        end if;
      end if;
      rd <= mem(to_integer(a(7 downto 2)));
      wait on clk, a;
    end loop;
  end process;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
use STD.TEXTIO.all;
entity imem is
    port(a: in std_logic_vector(5 downto 0);
         rd: out std_logic_vector(31 downto 0));
end;
architecture bheavioral of imem is
begin
  process is
    file mem_file: TEXT;
    variable L: line;
    variable ch: character;
    variable i, index, result : integer;
    type ramtype is array(63 downto 0) of std_logic_vector(31 downto 0);
    variable mem : ramtype;
  begin
    for i in 0 to 63 loop
      mem(i) := (others => '0');
    end loop;
    index := 0;
    FILE_OPEN(mem_file, "/tmp/memfile.data", READ_MODE);
    while not endfile(mem_file) loop
      readline(mem_file, L);
      result := 0;
      for i in 1 to 8 loop
        read(L, ch);
        if '0' <= ch and ch <= '9' then
          result := character'pos(ch) - character'pos('0');
        elsif 'a' <= ch and ch <= 'f' then
          result := character'pos(ch) - character'pos('a')+10;
        end if;
        mem(index)(35 - i*4 downto 32-i*4) := to_std_logic_vector(result,4);
      end loop;
      index := index + 1;
    end loop;
    loop
      rd <= mem(to_integer(a));
      wait on a;
    end loop;
  end process;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity top is
  port(clk, reset: in STD_LOGIC;
       writedata, dataddr : buffer STD_LOGIC_VECTOR(31 downto 0);
       memwrite : buffer STD_LOGIC
       );
end;
architecture behavioral of top is
  component cpu is
    port( clk : in std_logic;
          reset : in std_logic;
          instruction: in std_logic_vector(31 downto 0);
          in_data : in std_logic_vector(31 downto 0);
        
          pc_addr : out std_logic_vector(31 downto 0);
          data_addr : out std_logic_vector(31 downto 0);
          out_data : out std_logic_vector(31 downto 0);
          mem_write_enable : out std_logic
        );
  end component cpu;
  component imem
    port(a: in std_logic_vector(5 downto 0);
         rd: out std_logic_vector(31 downto 0));
  end component imem;
  
  component dmem
    port(clk, we: in std_logic;
         a, wd: in std_logic_vector(31 downto 0);
         rd : out std_logic_vector(31 downto 0));
  end component dmem;
  signal pc, instr, readdata: std_logic_vector(31 downto 0);
begin
  CPU1 : cpu
    port map(clk => clk,
             reset => reset,
             instruction => instr,
             in_data => readdata,
             
             pc_addr => pc,
             data_addr => dataddr,
             out_data => writedata,
             mem_write_enable => memwrite
             );
  IMEM_INST : imem
    port map(a => pc(7 downto 2), rd => instr);
  DMEM_INST : dmem
    port map(clk => clk, we => memwrite, a => dataddr, wd => writedata, rd => readdata);
             
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity mipstest is
--  Port ( );
end mipstest;

architecture behavioral of mipstest is
  component top
    port(clk, reset: in STD_LOGIC;
       writedata, dataddr : buffer STD_LOGIC_VECTOR(31 downto 0);
       memwrite : buffer STD_LOGIC
       );
  end component top;
  signal writedata, dataadr : std_logic_vector(31 downto 0);
  signal clk, reset, memwrite : std_logic;
begin
  DUT : top
    port map(clk => clk, reset => reset, writedata => writedata, dataddr => dataadr, memwrite => memwrite);
  process begin
    clk <= '1';
    wait for 5 ns;
    clk <= '0';
    wait for 5 ns;
  end process;
  
  process begin
    reset <= '1';
    wait for 22 ns;
    reset <= '0';
    wait;
  end process;
  
  process(clk) begin
    if(clk'event and clk ='0' and memwrite = '1') then
      if (to_integer(dataadr)=84 and to_integer(writedata)=7) then
        report "SIMULATION SUCCEEDED" severity failure;
      elsif (dataadr /= 80) then
        report "SIMULATION FAILED" severity failure;
      end if;
    end if;
  end process;
end behavioral;
*/