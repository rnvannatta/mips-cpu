----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 01/01/2020 08:48:52 PM
-- Design Name: 
-- Module Name: single_cycle - Behavioral
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
entity data_path is
  port( 
  
  
        clk : in std_logic;
        reset : in std_logic;
        pc_out : out std_logic_vector(31 downto 0);
        alu_out : out std_logic_vector(31 downto 0);
        write_data : out std_logic_vector(31 downto 0);
        instruction : in std_logic_vector(31 downto 0);
        read_data : in std_logic_vector(31 downto 0);
        
        reg_write_enable : in std_logic;
        r_instruction : in std_logic;
        alu_control : in std_logic_vector(2 downto 0);
        branch : in std_logic;
        jump : in std_logic;
        mem_to_reg : in std_logic
      );
end data_path;

architecture rtl of data_path is
  component flopr is
    generic ( WIDTH : natural := 32 );
    port( clk : in std_logic;
          reset : in std_logic := '0';
          enable : in std_logic := '1';
          output : out std_logic_vector(WIDTH-1 downto 0);
          input : in std_logic_vector(WIDTH-1 downto 0)
        );
  end component flopr;

  component register_file is
    port( clk : in std_logic;
          a1, a2 : in std_logic_vector(4 downto 0);
          w : in std_logic_vector(4 downto 0);
          write_data : in std_logic_vector(31 downto 0);
          write_enable : in std_logic;
          rd1, rd2 : out std_logic_vector(31 downto 0)
       ); 
  end component register_file;
  
  component sign_extend is
  port( output : out std_logic_vector(31 downto 0);
        input : in std_logic_vector(15 downto 0)
       );
  end component sign_extend;
  
  component shift_word_to_mem is
    port( output : out std_logic_vector(31 downto 0);
          input : in std_logic_vector(31 downto 0)
          );
  end component shift_word_to_mem;

  component jump_address is
    port( output : out std_logic_vector(31 downto 0);
          imm : in std_logic_vector(25 downto 0);
          pc_hi : in std_logic_vector(3 downto 0)
        );
  end component jump_address;

  component bimux is
    generic ( width : natural );
    port( output : out std_logic_vector(width-1 downto 0);
          a : in std_logic_vector(width-1 downto 0);
          b : in std_logic_vector(width-1 downto 0);
          y : in std_logic
        );
  end component bimux;
  
  component alu is
    port( control : in std_logic_vector(2 downto 0);
          a : in std_logic_vector(31 downto 0);
          b : in std_logic_vector(31 downto 0);
          c : out std_logic_vector(31 downto 0);
          z : out std_logic
        );
  end component alu;
  
  signal brach_mux_out : std_logic_vector(31 downto 0);
  signal jmp_addr : std_logic_vector(31 downto 0);
  signal next_pc : std_logic_vector(31 downto 0);
  signal pc_int : std_logic_vector(31 downto 0);
  signal inc_pc : std_logic_vector(31 downto 0);
  signal data_mem_read : std_logic_vector(31 downto 0);
  signal ext_imm : std_logic_vector(31 downto 0);
  signal shifted_imm : std_logic_vector(31 downto 0);
  
  signal reg_write : std_logic_vector(4 downto 0);
  signal reg_write_data : std_logic_vector(31 downto 0); 
  signal reg_read1 : std_logic_vector(31 downto 0);
  signal reg_read2 : std_logic_vector(31 downto 0);
  
  signal alu_input : std_logic_vector(31 downto 0);
  signal alu_res : std_logic_vector(31 downto 0);
  signal alu_zero : std_logic;
  
  signal alu_source : std_logic;
begin
  inc_pc <= pc_int + 4;
  PC_BRANCH_MUX : bimux
    generic map( width => 32) 
    port map( a => inc_pc,
              b => shifted_imm + inc_pc,
              output => brach_mux_out,
              y => alu_zero and branch
              );
  PC_JUMP_MUX : bimux
    generic map( width => 32) 
    port map( a => brach_mux_out,
              b => jmp_addr,
              output => next_pc,
              y => jump
              );  
  PC_INST : flopr
    port map( clk => clk,
              reset => reset,
              output => pc_int,
              input => next_pc );
  JMP_INST : jump_address
    port map( output => jmp_addr,
              imm => instruction(25 downto 0),
              pc_hi => pc_int(31 downto 28)
            );
            
  pc_out <= pc_int;
  alu_out <= alu_res;
  write_data <= reg_read2;
  data_mem_read <= read_data;
  
  REG_WRITE_IN_MUX : bimux
    generic map( width => 5 )
    port map( a => instruction(20 downto 16),
              b => instruction(15 downto 11),
              output => reg_write,
              y => r_instruction
             );
  REGISTER_INST : register_file
    port map( clk => clk,
              a1 => instruction(25 downto 21),
              a2 => instruction(20 downto 16),
              w => reg_write,
              write_data => reg_write_data,
              write_enable => reg_write_enable,
              rd1 => reg_read1,
              rd2 => reg_read2
              );
  SIGN_EXTEND_INST : sign_extend
    port map( output => ext_imm,
              input => instruction(15 downto 0)
              );
  SHIFT_LL_INST : shift_word_to_mem
    port map( output => shifted_imm,
              input => ext_imm );
  ALU_IN_MUX : bimux
    generic map( width => 32 )
    port map( a => reg_read2,
              b => ext_imm,
              output => alu_input,
              y => not r_instruction
              );
  ALU_INST : alu
    port map( control => alu_control,
              a => reg_read1,
              b => alu_input,
              c => alu_res,
              z => alu_zero
              );
  REG_WRITE_DATA_MUX : bimux
    generic map( width => 32 )
    port map( a => alu_res,
              b => data_mem_read,
              output => reg_write_data,
              y => mem_to_reg
              );
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity main_decode is
  port( 
        opcode : in std_logic_vector(5 downto 0);
        reg_write_enable : out std_logic;
        r_instruction : out std_logic;
        branch : out std_logic;
        jump : out std_logic;
        mem_write_enable : out std_logic;
        mem_to_reg : out std_logic;
        alu_op : out std_logic_vector(1 downto 0)
      );
end main_decode;

architecture rtl of main_decode is
begin
  process(all)
  begin
    case opcode is
      when "000000" => -- r-type
        reg_write_enable <= '1';
        r_instruction <= '1';
        branch <= '0';
        jump <= '0';
        mem_write_enable <= '0';
        mem_to_reg <= '0';
        alu_op <= "10";
      when "100011" => -- lw
        reg_write_enable <= '1';
        r_instruction <= '0';
        branch <= '0';
        jump <= '0';
        mem_write_enable <= '0';
        mem_to_reg <= '1';
        alu_op <= "00";   
      when "101011" => -- sw
        reg_write_enable <= '0';
        r_instruction <= '0';
        branch <= '0';
        jump <= '0';
        mem_write_enable <= '1';
        mem_to_reg <= '-';
        alu_op <= "00";   
      when "000100" => -- beq
        reg_write_enable <= '0';
        r_instruction <= '1';
        branch <= '1';
        jump <= '0';
        mem_write_enable <= '0';
        mem_to_reg <= '-';
        alu_op <= "01";    
      when "001000" => -- addi
        reg_write_enable <= '1';
        r_instruction <= '0';
        branch <= '0';
        jump <= '0';
        mem_write_enable <= '0';
        mem_to_reg <= '0';
        alu_op <= "00";       
      when "000010" => -- jmp
        reg_write_enable <= '0';
        r_instruction <= '-';
        branch <= '-';
        jump <= '1';
        mem_write_enable <= '0';
        mem_to_reg <= '-';
        alu_op <= "--";                 
      -- TODO implement SIGILL here
      when others =>
    end case;
  end process;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity alu_decode is
  port( funct : in std_logic_vector(5 downto 0);
        alu_op : in std_logic_vector(1 downto 0);
        alu_control : out std_logic_vector(2 downto 0)
      );
end alu_decode;

architecture rtl of alu_decode is
begin
  process(all)
  begin
   case alu_op is
      when "00" => alu_control <= "010"; -- add
      when "01" => alu_control <= "110"; -- sub
      when "10" =>
        case funct is
          when "100000" => alu_control <= "010"; -- add
          when "100010" => alu_control <= "110"; --sub 
          when "100100" => alu_control <= "000"; -- and
          when "100101" => alu_control <= "001"; -- or
          when "101010" => alu_control <= "111"; -- slt
          
          -- "011" is not used, currently the ALU returns all 0 in this case
          -- TODO: implement SIGILL here
          when others => alu_control <= "011";
          
        end case;
      when others => alu_control <= "011";
   end case;
  end process;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity control_unit is
  port( opcode : in std_logic_vector(5 downto 0);
        funct : in std_logic_vector(5 downto 0);
        reg_write_enable : out std_logic;
        r_instruction : out std_logic;
        alu_control : out std_logic_vector(2 downto 0);
        branch : out std_logic;
        jump : out std_logic;
        mem_write_enable : out std_logic;
        mem_to_reg : out std_logic
      );
end control_unit;

architecture rtl of control_unit is
  component main_decode is
    port( opcode : in std_logic_vector(5 downto 0);
          reg_write_enable : out std_logic;
          r_instruction : out std_logic;
          branch : out std_logic;
          jump : out std_logic;
          mem_write_enable : out std_logic;
          mem_to_reg : out std_logic;
          alu_op : out std_logic_vector(1 downto 0)
          );
  end component main_decode;
  
  component alu_decode is
    port( funct : in std_logic_vector(5 downto 0);
          alu_op : in std_logic_vector(1 downto 0);
          alu_control : out std_logic_vector(2 downto 0)
        );
  end component alu_decode;
  
  signal alu_op : std_logic_vector(1 downto 0);
begin
  MAIN_INST : main_decode
    port map( opcode => opcode,
              reg_write_enable => reg_write_enable,
              r_instruction => r_instruction,
              alu_op => alu_op,
              branch => branch,
              jump => jump,
              mem_write_enable => mem_write_enable,
              mem_to_reg => mem_to_reg
              );
  ALU_INST : alu_decode
    port map( funct => funct,
              alu_op => alu_op,
              alu_control => alu_control
              );
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity cpu is
  port( clk : in std_logic;
        reset : in std_logic;
        instruction: in std_logic_vector(31 downto 0);
        in_data : in std_logic_vector(31 downto 0);
        
        pc_addr : out std_logic_vector(31 downto 0);
        data_addr : out std_logic_vector(31 downto 0);
        out_data : out std_logic_vector(31 downto 0);
        mem_write_enable : out std_logic
      );
end cpu;

architecture rtl of cpu is
component control_unit is
  port( opcode : in std_logic_vector(5 downto 0);
        funct : in std_logic_vector(5 downto 0);
        reg_write_enable : out std_logic;
        r_instruction : out std_logic;
        alu_control : out std_logic_vector(2 downto 0);
        branch : out std_logic;
        jump : out std_logic;
        mem_write_enable : out std_logic;
        mem_to_reg : out std_logic
      );
end component control_unit;
component data_path is
  port( clk : in std_logic;
        reset : in std_logic;
        pc_out : out std_logic_vector(31 downto 0);
        alu_out : out std_logic_vector(31 downto 0);
        write_data : out std_logic_vector(31 downto 0);
        instruction : in std_logic_vector(31 downto 0);
        read_data : in std_logic_vector(31 downto 0);
        
        reg_write_enable : in std_logic;
        r_instruction : in std_logic;
        alu_control : in std_logic_vector(2 downto 0);
        branch : in std_logic;
        jump : in std_logic;
        mem_to_reg : in std_logic
      );
end component data_path;
  signal reg_write_enable : std_logic;
  signal r_instruction : std_logic;
  signal alu_control : std_logic_vector(2 downto 0);
  signal branch : std_logic;
  signal jump : std_logic;
  signal mem_to_reg : std_logic;
begin
  CONTROL_INST : control_unit
    port map( opcode => instruction(31 downto 26),
              funct => instruction(5 downto 0),
              mem_write_enable => mem_write_enable,
              
              reg_write_enable => reg_write_enable,
              r_instruction => r_instruction,
              alu_control => alu_control,
              branch => branch,
              jump => jump,
              mem_to_reg => mem_to_reg
            );
  DATAPATH_INST : data_path
    port map( clk => clk,
              reset => reset,
              
              pc_out => pc_addr,
              alu_out => data_addr,
              write_data => out_data,
              instruction => instruction,
              read_data => in_data,
              
              reg_write_enable => reg_write_enable,
              r_instruction => r_instruction,
              alu_control => alu_control,
              branch => branch,
              jump => jump,
              mem_to_reg => mem_to_reg
            );
end rtl;