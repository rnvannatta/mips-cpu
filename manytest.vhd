----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 01/10/2020 11:01:04 PM
-- Design Name: 
-- Module Name: manytest - behavioral
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


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
use STD.TEXTIO.all;
entity mem is
    port(clk, we : in std_logic;
         a: in std_logic_vector(5 downto 0);
         wd : in std_logic_vector(31 downto 0);
         rd: out std_logic_vector(31 downto 0));
end;

architecture behavioral of mem is
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
    FILE_OPEN(mem_file, "/home/vannatta/mipprog.dat", READ_MODE);
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
      if(rising_edge(clk)) then
        if(we='1') then
          mem(to_integer(a)) := wd;
        end if;
      end if;
      rd <= mem(to_integer(a));
      wait on clk, a;
    end loop;
  end process;
end;

/*
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;
entity top_mc is
  port(clk, reset: in STD_LOGIC;
       writedata, dataddr : buffer STD_LOGIC_VECTOR(31 downto 0);
       memwrite : buffer STD_LOGIC
       );
end;
architecture behavioral of top_mc is
component cpu_mc is
  port( clk : in std_logic;
        reset : in std_logic;
        write_data : out std_logic_vector(31 downto 0); -- data to be written to memory
        read_data : in std_logic_vector(31 downto 0); -- data to be read from memory
        data_addr : out std_logic_vector(31 downto 0); -- data address to write to or read from
        mem_write_enable : out std_logic
      );
end component;
component mem is
    port(clk, we : in std_logic;
         a: in std_logic_vector(5 downto 0);
         wd : in std_logic_vector(31 downto 0);
         rd: out std_logic_vector(31 downto 0));
end component;
  signal readdata: std_logic_vector(31 downto 0);
begin
  CPU1 : cpu_mc
    port map(clk => clk,
             reset => reset,
             write_data => writedata,
             read_data => readdata,
             data_addr => dataddr,
             mem_write_enable => memwrite
             );
  MEM_INST : mem
    port map(clk => clk, we => memwrite, a => dataddr(7 downto 2), wd => writedata, rd => readdata);
             
end;
*/

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD_UNSIGNED.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity manytest is
--  Port ( );
end manytest;

architecture behavioral of manytest is
  component top_pipe
    port(clk, reset: in STD_LOGIC;
       
       led : out std_logic_vector(3 downto 0)
       );
  end component;
  signal clk, reset : std_logic;
  signal led : std_logic_vector(3 downto 0);
begin
  DUT : top_pipe
    port map(clk => clk, reset => reset, led => led);
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
  
  /*
  process(clk) begin
    if(clk'event and clk ='0' and memwrite = '1') then
      if (to_integer(dataadr)=252 and to_integer(writedata)=7) then
        report "SIMULATION SUCCEEDED" severity failure;
      elsif (dataadr /= 128) then
        report "SIMULATION FAILED" severity failure;
      end if;
    end if;
  end process;
  */
  process(led) begin
    if led = "0111" then
        report "SIMULATION SUCCEEDED" severity failure;
    end if;
  end process;
end behavioral;
