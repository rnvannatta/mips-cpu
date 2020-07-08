----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 01/12/2020 02:52:31 PM
-- Design Name: 
-- Module Name: top_pipe - Behavioral
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
entity flopr is
  generic ( WIDTH : natural := 32 );
  port( clk : in std_logic;
        reset : in std_logic := '0';
        clear : in std_logic := '0';
        enable : in std_logic := '1';
        output : out std_logic_vector(WIDTH-1 downto 0) := (others => '0');
        input : in std_logic_vector(WIDTH-1 downto 0)
       );
end flopr;

architecture rtl of flopr is
begin
  process(clk, reset)
  begin
    if reset = '1' then
      output <= (others => '0');
    elsif rising_edge(clk) then
      if clear = '1' then
        output <= (others => '0');
      elsif enable = '1' then
        output <= input;
      end if;
    end if;
  end process;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity register_file is
  port( clk : in std_logic;
        a1, a2 : in std_logic_vector(4 downto 0);
        w : in std_logic_vector(4 downto 0);
        write_data : in std_logic_vector(31 downto 0);
        write_enable : in std_logic;
        rd1, rd2 : out std_logic_vector(31 downto 0)
       ); 
end register_file;

architecture rtl of register_file is
  type t_registers is array (31 downto 0) of std_logic_vector(31 downto 0);
  signal registers : t_registers;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      if write_enable = '1' then
        registers(to_integer(w)) <= write_data;
      end if;
    end if;
  end process;
  process(all)
  begin
    if(to_integer(a1) = 0) then
      rd1 <= X"00000000";
    else
      if w = a1 and write_enable = '1' then
        rd1 <= write_data;
      else
        rd1 <= registers(to_integer(a1));
      end if;
    end if;
    if(to_integer(a2) = 0) then
      rd2 <= X"00000000";
    else
      if w = a2 and write_enable = '1' then
        rd2 <= write_data;
      else
        rd2 <= registers(to_integer(a2));
      end if;
    end if;
  end process;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity sign_extend is
  port( output : out std_logic_vector(31 downto 0);
        input : in std_logic_vector(15 downto 0)
       );
end sign_extend;

architecture rtl of sign_extend is
begin
  output <= X"FFFF" & input when input(15) = '1' else X"0000" & input;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity shift_word_to_mem is
  port( output : out std_logic_vector(31 downto 0);
        input : in std_logic_vector(31 downto 0)
       );
end shift_word_to_mem;

architecture rtl of shift_word_to_mem is
begin
  output <= input(29 downto 0) & "00";
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity jump_address is
  port( output : out std_logic_vector(31 downto 0);
        imm : in std_logic_vector(25 downto 0);
        pc_hi : in std_logic_vector(3 downto 0)
       );
end jump_address;

architecture rtl of jump_address is
begin
  output <= pc_hi & imm(25 downto 0) & "00";
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity bimux is
  generic ( width : natural );
  port( output : out std_logic_vector(width-1 downto 0);
        a : in std_logic_vector(width-1 downto 0);
        b : in std_logic_vector(width-1 downto 0);
        y : in std_logic
       );
end bimux;

architecture rtl of bimux is
begin
  --output <= b when y else a;
  process(all)
  begin
    if y = '1' then
      output <= b;
    else
      output <= a;
    end if;
  end process;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity qmux is
  generic ( width : natural );
  port( output : out std_logic_vector(width-1 downto 0);
        a : in std_logic_vector(width-1 downto 0);
        b : in std_logic_vector(width-1 downto 0);
        c : in std_logic_vector(width-1 downto 0);
        d : in std_logic_vector(width-1 downto 0);
        y : in std_logic_vector(1 downto 0)
       );
end qmux;
architecture rtl of qmux is
begin
  --output <= b when y else a;
  process(all)
  begin
    case y is
      when "00" => output <= a;
      when "01" => output <= b;
      when "10" => output <= c;
      when "11" => output <= d;
      when others => output <= (others => '0');
    end case;
  end process;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity carrysave is
  generic( WIDTH : natural := 32 );
  port( a, b, cin : in std_logic_vector(WIDTH-1 downto 0);
        sum : out std_logic_vector(WIDTH-1 downto 0);
        cout : out std_logic_vector(WIDTH downto 0)
        );
end;
architecture rtl of carrysave is
begin
  sum <= a xor (b xor cin);
  cout(WIDTH downto 1) <= (a and b) or (b and cout) or (a and cout);
  cout(0) <= '0';
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity multiplier is
  port( a, b : in std_logic_vector(31 downto 0);
        add : in std_logic_vector(63 downto 0);
        madd : out std_logic_vector(63 downto 0)
        );
end;
architecture rtl of multiplier is
  type t_pass1 is array (32 downto 0) of std_logic_vector(63 downto 0);
  signal pass1_a : t_pass1;
  
  type t_pass2 is array(21 downto 0) of std_logic_vector(63 downto 0);
  signal pass2_a : t_pass2;
begin
  GENERATE_PEASENT : for i in 0 to 31 generate
    pass1_a(i) <= (31 + i downto i => b, others => '0') when a(i) = '1' else (others => '0');
  end generate;
    pass1_a(32) <= add;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
package cpu_basics is
  component flopr is
    generic ( WIDTH : natural := 32 );
    port( clk : in std_logic;
        reset : in std_logic := '0';
        clear : in std_logic := '0';
        enable : in std_logic := '1';
        output : out std_logic_vector(WIDTH-1 downto 0) := (others => '0');
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
  component bimux is
    generic ( width : natural );
    port( output : out std_logic_vector(width-1 downto 0);
          a : in std_logic_vector(width-1 downto 0);
          b : in std_logic_vector(width-1 downto 0);
          y : in std_logic
        );
  end component bimux;
  component qmux is
    generic ( width : natural );
    port( output : out std_logic_vector(width-1 downto 0);
          a : in std_logic_vector(width-1 downto 0);
          b : in std_logic_vector(width-1 downto 0);
          c : in std_logic_vector(width-1 downto 0);
          d : in std_logic_vector(width-1 downto 0);
          y : in std_logic_vector(1 downto 0)
        );
  end component qmux;
  component jump_address is
    port( output : out std_logic_vector(31 downto 0);
          imm : in std_logic_vector(25 downto 0);
          pc_hi : in std_logic_vector(3 downto 0)
        );
  end component jump_address;
end package;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity alu is
  port( control : in std_logic_vector(2 downto 0);
        a : in std_logic_vector(31 downto 0);
        b : in std_logic_vector(31 downto 0);
        c : out std_logic_vector(31 downto 0);
        z : out std_logic
      );
end alu;
architecture rtl of alu is
begin
  process(all)
    variable tmp : std_logic_vector(31 downto 0);
  begin
    case control is
      when "000" => tmp := a and b;
      when "001" => tmp := a or b;
      when "010" => tmp := a + b;
      when "011" => tmp := X"00000000"; -- unused
      when "100" => tmp := a and not b;
      when "101" => tmp := a or not b;
      when "110" => tmp := a - b;
      when "111" =>
        tmp := a - b;
      when others => tmp := (others => '0');
    end case;
    if control = "111" then
        if tmp(31) = '1' then
          c <= X"00000001";
        else
          c <= (others => '0');
        end if;
    else
        c <= tmp;
    end if;
    z <= not (or tmp);
  end process;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library work;
use work.cpu_basics.all;
entity decode_in_registers is
  port( clk, enable, clear, reset : in std_logic;
        instruction_f, inc_pc_f : in std_logic_vector(31 downto 0);
        instruction_d, inc_pc_d : out std_logic_vector(31 downto 0)
       );
end;
architecture rtl of decode_in_registers is
  signal held_clear, held_enable : std_logic;
  signal held_instruction, tmp_instruction : std_logic_vector(31 downto 0);
begin
  INSTR_INST : flopr
    port map( clk => clk, enable => enable, clear => clear, reset => reset, input => instruction_f, output => instruction_d );
  --instruction_d <= instruction_f;
  INC_PC_INST : flopr
    port map( clk => clk, enable => enable, input => inc_pc_f, output => inc_pc_d );
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library work;
use work.cpu_basics.all;
entity execute_in_registers is
  port( clk, clear, reset : in std_logic;
        reg_read1_d, reg_read2_d, ext_imm_d, inc_pc_d : in std_logic_vector(31 downto 0);
        reg_read1_e, reg_read2_e, ext_imm_e, inc_pc_e : out std_logic_vector(31 downto 0);
        
        reg_s_d, reg_t_d, reg_d_d : in std_logic_vector(4 downto 0);
        reg_s_e, reg_t_e, reg_d_e : out std_logic_vector(4 downto 0);
        
        reg_write_enable_d, mem_write_enable_d, branch_d : in std_logic;
        reg_write_enable_e, mem_write_enable_e, branch_e : out std_logic;
        
        mem_to_reg_d, alu_source_d, reg_dest_d : in std_logic;
        mem_to_reg_e, alu_source_e, reg_dest_e : out std_logic;
        
        alu_control_d : in std_logic_vector(2 downto 0);
        alu_control_e : out std_logic_vector(2 downto 0)
       );
end;
architecture rtl of execute_in_registers is
  signal flag_inputs : std_logic_vector(5 downto 0);
  signal flag_outputs : std_logic_Vector(5 downto 0);
  
begin
  REG_READ1_INST : flopr
    port map( clk => clk, input => reg_read1_d, output => reg_read1_e );
  REG_READ2_INST : flopr
    port map( clk => clk, input => reg_read2_d, output => reg_read2_e );
  EXT_IMM_INST : flopr
    port map( clk => clk, input => ext_imm_d, output => ext_imm_e );
  INC_PC_INST : flopr
    port map( clk => clk, input => inc_pc_d, output => inc_pc_e );
  REG_S_INST : flopr
    generic map( WIDTH => 5 )
    port map( clk => clk, clear => clear, reset => reset, input => reg_s_d, output => reg_s_e );
  REG_T_INST : flopr
    generic map( WIDTH => 5 )
    port map( clk => clk, clear => clear, reset => reset, input => reg_t_d, output => reg_t_e );
  REG_D_INST : flopr
    generic map( WIDTH => 5 )
    port map( clk => clk, clear => clear, reset => reset, input => reg_d_d, output => reg_d_e );
    
  flag_inputs <= (reg_write_enable_d, mem_write_enable_d, branch_d, mem_to_reg_d, alu_source_d, reg_dest_d);
                 (reg_write_enable_e, mem_write_enable_e, branch_e, mem_to_reg_e, alu_source_e, reg_dest_e) <= flag_outputs;
  FLAG_INST : flopr
    generic map( WIDTH => 6 )
    port map( clk => clk,
              clear => clear,
              reset => reset,
              input => flag_inputs,
              output => flag_outputs
            );
  ALU_CONTROL_INST : flopr
    generic map( WIDTH => 3)
    port map( clk => clk, input => alu_control_d, output => alu_control_e );
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library work;
use work.cpu_basics.all;
entity memory_in_registers is
  port( clk, clear, reset : in std_logic;
        alu_out_e, write_data_e, pc_branch_e : in std_logic_vector(31 downto 0);
        alu_out_m, write_data_m, pc_branch_m : out std_logic_vector(31 downto 0);
        
        write_reg_e : in std_logic_vector(4 downto 0);
        write_reg_m : out std_logic_vector(4 downto 0);
        
        zero_flag_e : in std_logic;
        zero_flag_m : out std_logic;
        
        reg_write_enable_e, mem_write_enable_e, branch_e : in std_logic;
        reg_write_enable_m, mem_write_enable_m, branch_m : out std_logic;
        
        mem_to_reg_e : in std_logic;
        mem_to_reg_m : out std_logic
       );
end;
architecture rtl of memory_in_registers is
  signal flag_inputs : std_logic_vector(3 downto 0);
  signal flag_outputs : std_logic_Vector(3 downto 0);
begin
  ALU_OUT_INST : flopr
    port map( clk => clk, input => alu_out_e, output => alu_out_m );
  WRITE_DATA_INST : flopr
    port map( clk => clk, input => write_data_e, output => write_data_m );
  PC_BRANCH_INST : flopr
    port map( clk => clk, input => pc_branch_e, output => pc_branch_m );
  WRITE_REG_INST : flopr
    generic map( WIDTH => 5 )
    port map( clk => clk, clear => clear, reset => reset, input => write_reg_e, output => write_reg_m );
  ZERO_FLAG_INST : flopr
    generic map( WIDTH => 1 )
    port map( clk => clk, input => (0 => zero_flag_e), output(0) => zero_flag_m );
  
  flag_inputs <= (reg_write_enable_e, mem_write_enable_e, branch_e, mem_to_reg_e);
                 (reg_write_enable_m, mem_write_enable_m, branch_m, mem_to_reg_m) <= flag_outputs;
  FLAG_INST : flopr
    generic map( WIDTH => 4 )
    port map( clk => clk,
              clear => clear,
              reset => reset,
              input => flag_inputs,
              output => flag_outputs
             );
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library work;
use work.cpu_basics.all;
entity writeback_in_registers is
  port( clk, reset : in std_logic;
        alu_out_m, read_data_m : in std_logic_vector(31 downto 0);
        alu_out_w, read_data_w : out std_logic_vector(31 downto 0);
        
        write_reg_m : in std_logic_vector(4 downto 0);
        write_reg_w : out std_logic_vector(4 downto 0);
        
        reg_write_enable_m : in std_logic;
        reg_write_enable_w : out std_logic;
        
        mem_to_reg_m : in std_logic;
        mem_to_reg_w : out std_logic
       );
end;
architecture rtl of writeback_in_registers is
  signal flag_inputs : std_logic_vector(1 downto 0);
  signal flag_outputs : std_logic_Vector(1 downto 0);
begin
  ALU_OUT_INST : flopr
    port map( clk => clk, input => alu_out_m, output => alu_out_w );
  READ_DATA_INST : flopr
    port map( clk => clk, input => read_data_m, output => read_data_w );
  WRITE_REG_INST : flopr
    generic map( WIDTH => 5 )
    port map( clk => clk, reset => reset, input => write_reg_m, output => write_reg_w );
  flag_inputs <= (reg_write_enable_m, mem_to_reg_m);
                 (reg_write_enable_w, mem_to_reg_w) <= flag_outputs;
  FLAG_INST : flopr
    generic map( WIDTH => 2 )
    port map( clk => clk,
              reset => reset,
              input => flag_inputs,
              output => flag_outputs
            );
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
library work;
use work.cpu_basics.all;
entity data_path_pipe is
  port( clk : in std_logic;
        reset : in std_logic;
        
        inst_addr_f : out std_logic_vector(31 downto 0);
        instruction_f : in std_logic_vector(31 downto 0);
        instruction_d_out : out std_logic_vector(31 downto 0);
        
        mem_addr_m : out std_logic_vector(31 downto 0);
        read_data_m : in std_logic_vector(31 downto 0);
        
        mem_write_data_m : out std_logic_vector(31 downto 0);
        mem_write_enable_m : out std_logic;
        
        reg_write_enable_d, mem_write_enable_d, branch_d, jump_d : in std_logic;
        mem_to_reg_d, alu_source_d, reg_dest_d : in std_logic;
        alu_control_d : in std_logic_vector(2 downto 0);
        
        reg_s_hazard_e, reg_t_hazard_e : out std_logic_vector(4 downto 0);
        reg_write_hazard_m, reg_write_hazard_w : out std_logic;
        hazard_data_read_writeback_w, hazard_alu_out_writeback_m : out std_logic_vector(4 downto 0);
        hazard_forward_alu_a, hazard_forward_alu_b : in std_logic_vector(1 downto 0);
        
        stall_f, stall_d, flush_d, flush_e, flush_m : in std_logic;
        reg_s_hazard_d, reg_t_hazard_d : out std_logic_vector(4 downto 0);
        mem_to_reg_hazard_e, branch_hazard_m : out std_logic
      );
end;

architecture rtl of data_path_pipe is
  component alu is
    port( control : in std_logic_vector(2 downto 0);
          a : in std_logic_vector(31 downto 0);
          b : in std_logic_vector(31 downto 0);
          c : out std_logic_vector(31 downto 0);
          z : out std_logic
        );
  end component alu;
  component decode_in_registers is
    port( clk, enable, clear, reset : in std_logic;
          instruction_f, inc_pc_f : in std_logic_vector(31 downto 0);
          instruction_d, inc_pc_d : out std_logic_vector(31 downto 0)
         );
  end component;
  component execute_in_registers is
    port( clk, clear, reset : in std_logic;
          reg_read1_d, reg_read2_d, ext_imm_d, inc_pc_d : in std_logic_vector(31 downto 0);
          reg_read1_e, reg_read2_e, ext_imm_e, inc_pc_e : out std_logic_vector(31 downto 0);
        
          reg_s_d, reg_t_d, reg_d_d : in std_logic_vector(4 downto 0);
          reg_s_e, reg_t_e, reg_d_e : out std_logic_vector(4 downto 0);
        
          reg_write_enable_d, mem_write_enable_d, branch_d : in std_logic;
          reg_write_enable_e, mem_write_enable_e, branch_e : out std_logic;
        
          mem_to_reg_d, alu_source_d, reg_dest_d : in std_logic;
          mem_to_reg_e, alu_source_e, reg_dest_e : out std_logic;
        
          alu_control_d : in std_logic_vector(2 downto 0);
          alu_control_e : out std_logic_vector(2 downto 0)
         );
  end component;
  component memory_in_registers is
    port( clk, clear, reset : in std_logic;
          alu_out_e, write_data_e, pc_branch_e : in std_logic_vector(31 downto 0);
          alu_out_m, write_data_m, pc_branch_m : out std_logic_vector(31 downto 0);
        
          write_reg_e : in std_logic_vector(4 downto 0);
          write_reg_m : out std_logic_vector(4 downto 0);
        
          zero_flag_e : in std_logic;
          zero_flag_m : out std_logic;
        
          reg_write_enable_e, mem_write_enable_e, branch_e : in std_logic;
          reg_write_enable_m, mem_write_enable_m, branch_m : out std_logic;
        
          mem_to_reg_e : in std_logic;
          mem_to_reg_m : out std_logic
         );
  end component;
  component writeback_in_registers is
    port( clk, reset : in std_logic;
          alu_out_m, read_data_m : in std_logic_vector(31 downto 0);
          alu_out_w, read_data_w : out std_logic_vector(31 downto 0);
        
          write_reg_m : in std_logic_vector(4 downto 0);
          write_reg_w : out std_logic_vector(4 downto 0);
        
          reg_write_enable_m : in std_logic;
          reg_write_enable_w : out std_logic;
        
          mem_to_reg_m : in std_logic;
          mem_to_reg_w : out std_logic
        );
  end component;
  signal pc_f, inc_pc_f : std_logic_vector(31 downto 0);
  signal next_pc_f : std_logic_vector(31 downto 0);
  
  signal instruction_d, inc_pc_d : std_logic_vector(31 downto 0);
  signal clear_d : std_logic;
  signal jmp_addr_d : std_logic_vector(31 downto 0);
  
  signal reg_read1_d, reg_read2_d, ext_imm_d : std_logic_vector(31 downto 0);
  signal reg_read1_e, reg_read2_e, ext_imm_e, inc_pc_e : std_logic_vector(31 downto 0);
  signal reg_s_e, reg_t_e, reg_d_e : std_logic_vector(4 downto 0);
        
  signal reg_write_enable_e, mem_write_enable_e, branch_e : std_logic;
  signal mem_to_reg_e, alu_source_e, reg_dest_e : std_logic;
  signal alu_control_e : std_logic_vector(2 downto 0);
  signal alu_a_e, alu_b_e : std_logic_vector(31 downto 0);
  
  signal alu_out_e, write_data_e : std_logic_vector(31 downto 0);
  signal alu_out_m, pc_branch_m : std_logic_vector(31 downto 0);
  signal write_reg_e : std_logic_vector(4 downto 0);
  signal write_reg_m : std_logic_vector(4 downto 0);
  signal reg_write_enable_m, branch_m : std_logic;
  signal mem_to_reg_m : std_logic;
  signal zero_flag_e : std_logic;
  signal zero_flag_m : std_logic;
  
  signal alu_out_w, read_data_w : std_logic_vector(31 downto 0);
  signal reg_write_enable_w, mem_to_reg_w : std_logic;
  signal write_reg_w : std_logic_vector(4 downto 0);
  
  signal alu_input2_e : std_logic_vector(31 downto 0);
  signal shifted_imm_e : std_logic_vector(31 downto 0);
  
  signal brach_mux_m : std_logic_vector(31 downto 0);
  signal reg_data_w : std_logic_vector(31 downto 0);
  
  signal take_branch_m : std_logic;
begin
  inc_pc_f <= pc_f + 4;
  inst_addr_f <= pc_f;
  PC_BRANCH_MUX : bimux
    generic map( width => 32) 
    port map( a => inc_pc_f,
              b => pc_branch_m,
              output => brach_mux_m,
              y => take_branch_m
              );
  PC_JUMP_MUX : bimux
    generic map( width => 32) 
    port map( a => brach_mux_m,
              b => jmp_addr_d,
              output => next_pc_f,
              y => jump_d
              );  
  PC_INST : flopr
    port map( clk => clk,
              enable => not stall_f,
              reset => reset,
              output => pc_f,
              input => next_pc_f );
  DECODE_REG_INST : decode_in_registers
    port map( clk => clk,
              enable => not stall_d,
              clear => flush_d,
              reset => reset,
              instruction_f => instruction_f,
              instruction_d => open,
              inc_pc_f => inc_pc_f,
              inc_pc_d => inc_pc_d
              );
  instruction_d <= instruction_f;
  REGISTER_INST : register_file
    port map( clk => clk,
              a1 => instruction_d(25 downto 21),
              a2 => instruction_d(20 downto 16),
              w => write_reg_w,
              write_data => reg_data_w,
              write_enable => reg_write_enable_w,
              rd1 => reg_read1_d,
              rd2 => reg_read2_d
              );
  SIGN_EXTEND_INST : sign_extend
    port map( output => ext_imm_d,
              input => instruction_d(15 downto 0)
              );
  JMP_INST : jump_address
    port map( output => jmp_addr_d,
              imm => instruction_d(25 downto 0),
              pc_hi => inc_pc_d(31 downto 28)
            );
  reg_s_hazard_d <= instruction_d(25 downto 21);
  reg_t_hazard_d <= instruction_d(20 downto 16);
  instruction_d_out <= instruction_d;
  EXECUTE_REG_INST : execute_in_registers
    port map( clk => clk,
              clear => flush_e,
              reset => reset,
              reg_read1_d => reg_read1_d,
              reg_read2_d => reg_read2_d,
              ext_imm_d => ext_imm_d,
              inc_pc_d => inc_pc_d,
              reg_read1_e => reg_read1_e,
              reg_read2_e => reg_read2_e,
              ext_imm_e => ext_imm_e,
              inc_pc_e => inc_pc_e,
              
              reg_s_d => instruction_d(25 downto 21),
              reg_t_d => instruction_d(20 downto 16),
              reg_d_d => instruction_d(15 downto 11),
              reg_s_e => reg_s_e,
              reg_t_e => reg_t_e,
              reg_d_e => reg_d_e,
              
              reg_write_enable_d => reg_write_enable_d,
              mem_write_enable_d => mem_write_enable_d,
              branch_d => branch_d,
              reg_write_enable_e => reg_write_enable_e,
              mem_write_enable_e => mem_write_enable_e,
              branch_e => branch_e,
              
              mem_to_reg_d => mem_to_reg_d,
              alu_source_d => alu_source_d,
              reg_dest_d => reg_dest_d,
              mem_to_reg_e => mem_to_reg_e,
              alu_source_e => alu_source_e,
              reg_dest_e => reg_dest_e,
              
              alu_control_d => alu_control_d,
              alu_control_e => alu_control_e
              );  
  
  reg_s_hazard_e <= reg_s_e;
  reg_t_hazard_e <= reg_t_e;
  mem_to_reg_hazard_e <= mem_to_reg_e;
  
  ALU_A_MUX : qmux
    generic map( width => 32 )
    port map( a => reg_read1_e,
              b => reg_data_w,
              c => alu_out_m,
              d => (others => 'X'),
              output => alu_a_e,
              y => hazard_forward_alu_a
              );
  ALU_B_MUX : qmux
    generic map( width => 32 )
    port map( a => reg_read2_e,
              b => reg_data_w,
              c => alu_out_m,
              d => (others => 'X'),
              output => alu_b_e,
              y => hazard_forward_alu_b
              );
  
  ALU_IN_MUX : bimux
    generic map( width => 32 )
    port map( a => alu_b_e,
              b => ext_imm_e,
              output => alu_input2_e,
              y => alu_source_e
              );
  ALU_INST : alu
    port map( control => alu_control_e,
              a => alu_a_e,
              b => alu_input2_e,
              c => alu_out_e,
              z => zero_flag_e
              );
  
  REG_WRITE_IN_MUX : bimux
    generic map( width => 5 )
    port map( a => reg_t_e,
              b => reg_d_e,
              output => write_reg_e,
              y => reg_dest_e
             );
  SHIFT_LL_INST : shift_word_to_mem
    port map( output => shifted_imm_e,
              input => ext_imm_e );
  
  write_data_e <= alu_b_e;
              
  MEMORY_REGI_INST : memory_in_registers
    port map( clk => clk,
              clear => flush_m,
              reset => reset,
              alu_out_e => alu_out_e,
              write_data_e => write_data_e,
              pc_branch_e => shifted_imm_e + inc_pc_e,
              alu_out_m => alu_out_m,
              write_data_m => mem_write_data_m,
              pc_branch_m => pc_branch_m,
              
              write_reg_e => write_reg_e,
              write_reg_m => write_reg_m,
              
              zero_flag_e => zero_flag_e,
              zero_flag_m => zero_flag_m,
              
              reg_write_enable_e => reg_write_enable_e,
              mem_write_enable_e => mem_write_enable_e,
              branch_e => branch_e,
              reg_write_enable_m => reg_write_enable_m,
              mem_write_enable_m => mem_write_enable_m,
              branch_m => branch_m,
              mem_to_reg_e => mem_to_reg_e,
              mem_to_reg_m => mem_to_reg_m
              );  
              
  hazard_alu_out_writeback_m <= write_reg_m;
  reg_write_hazard_m <= reg_write_enable_m;
  take_branch_m <= zero_flag_m and branch_m;
  branch_hazard_m <= take_branch_m;
              
  WRITEBACK_REG_INST : writeback_in_registers
    port map( clk => clk,
              reset => reset,
              alu_out_m => alu_out_m,
              read_data_m => read_data_m,
              alu_out_w => alu_out_w,
              read_data_w => open,
              
              write_reg_m => write_reg_m,
              write_reg_w => write_reg_w,
              
              reg_write_enable_m => reg_write_enable_m,
              reg_write_enable_w => reg_write_enable_w,
              
              mem_to_reg_m => mem_to_reg_m,
              mem_to_reg_w => mem_to_reg_w
            );
  read_data_w <= read_data_m;
  mem_addr_m <= alu_out_m;
              
  hazard_data_read_writeback_w <= write_reg_w;
  reg_write_hazard_w <= reg_write_enable_w;
  
  REG_WRITE_DATA_MUX : bimux
    generic map( width => 32 )
    port map( a => alu_out_w,
              b => read_data_w,
              output => reg_data_w,
              y => mem_to_reg_w
              );
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity hazard_unit is
  port( reg_s_e, reg_t_e : in std_logic_vector(4 downto 0);
        reg_write_m, reg_write_w : in std_logic;
        hazard_alu_out_writeback_m, hazard_data_read_writeback_w : in std_logic_vector(4 downto 0);
        hazard_forward_alu_a, hazard_forward_alu_b : out std_logic_vector(1 downto 0);
        
        stall_f, stall_d, flush_d, flush_e, flush_m : out std_logic;
        reg_s_hazard_d, reg_t_hazard_d : in std_logic_vector(4 downto 0);
        mem_to_reg_hazard_e, branch_m, jump_d : in std_logic
        ); 
end;
architecture rtl of hazard_unit is
begin
  process(all)
    variable lwstall : std_logic;
  begin
    -- Forward from either the alu or memory read to register argument s
    if reg_s_e /= "00000" and reg_s_e = hazard_alu_out_writeback_m and reg_write_m = '1' then
      hazard_forward_alu_a <= "10";
    elsif reg_s_e /= "00000" and reg_s_e = hazard_data_read_writeback_w and reg_write_w = '1' then
      hazard_forward_alu_a <= "01";
    else
      hazard_forward_alu_a <= "00";
    end if;
    -- Forward from either the alu or memory read to register argument t
    if reg_t_e /= "00000" and reg_t_e = hazard_alu_out_writeback_m and reg_write_m = '1' then
      hazard_forward_alu_b <= "10";
    elsif reg_t_e /= "00000" and reg_t_e = hazard_data_read_writeback_w and reg_write_w = '1' then
      hazard_forward_alu_b <= "01";
    else
      hazard_forward_alu_b <= "00";
    end if;
    -- stall the pipeline for reads
    lwstall := '1' when mem_to_reg_hazard_e = '1' and (reg_s_hazard_d = reg_t_e or reg_t_hazard_d = reg_t_e) else '0';
    stall_f <= lwstall;
    stall_d <= lwstall;
    flush_d <= branch_m or jump_d;
    flush_e <= lwstall or branch_m;
    flush_m <= branch_m;
  end process;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity main_decode_pipe is
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
end main_decode_pipe;

architecture rtl of main_decode_pipe is
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
        branch <= '0';
        jump <= '1';
        mem_write_enable <= '0';
        mem_to_reg <= '-';
        alu_op <= "--";                 
      -- TODO implement SIGILL here
      when others =>
        reg_write_enable <= '-';
        r_instruction <= '-';
        branch <= '-';
        jump <= '-';
        mem_write_enable <= '-';
        mem_to_reg <= '-';
        alu_op <= "--";       
    end case;
  end process;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity alu_decode_pipe is
  port( funct : in std_logic_vector(5 downto 0);
        alu_op : in std_logic_vector(1 downto 0);
        alu_control : out std_logic_vector(2 downto 0)
      );
end;

architecture rtl of alu_decode_pipe is
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
entity control_unit_pipe is
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
end;

architecture rtl of control_unit_pipe is
  component main_decode_pipe is
    port( opcode : in std_logic_vector(5 downto 0);
          reg_write_enable : out std_logic;
          r_instruction : out std_logic;
          branch : out std_logic;
          jump : out std_logic;
          mem_write_enable : out std_logic;
          mem_to_reg : out std_logic;
          alu_op : out std_logic_vector(1 downto 0)
          );
  end component;
  
  component alu_decode_pipe is
    port( funct : in std_logic_vector(5 downto 0);
          alu_op : in std_logic_vector(1 downto 0);
          alu_control : out std_logic_vector(2 downto 0)
        );
  end component;
  signal alu_op : std_logic_vector(1 downto 0);
begin
  MAIN_INST : main_decode_pipe
    port map( opcode => opcode,
              reg_write_enable => reg_write_enable,
              r_instruction => r_instruction,
              alu_op => alu_op,
              branch => branch,
              jump => jump,
              mem_write_enable => mem_write_enable,
              mem_to_reg => mem_to_reg
              );
  ALU_INST : alu_decode_pipe
    port map( funct => funct,
              alu_op => alu_op,
              alu_control => alu_control
              );
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity cpu_pipe is
  port( clk : in std_logic;
        reset : in std_logic;

        inst_addr : out std_logic_vector(31 downto 0);
        instruction : in std_logic_vector(31 downto 0);
        
        mem_addr : out std_logic_vector(31 downto 0);
        read_data : in std_logic_vector(31 downto 0);
        
        mem_write_data : out std_logic_vector(31 downto 0);
        mem_write_enable : out std_logic;
        
        flush_instr, stall_instr : out std_logic
      );
end;

architecture rtl of cpu_pipe is
component control_unit_pipe is
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
end component control_unit_pipe;
component data_path_pipe is
  port( clk : in std_logic;
        reset : in std_logic;
        
        inst_addr_f : out std_logic_vector(31 downto 0);
        instruction_f : in std_logic_vector(31 downto 0);
        instruction_d_out : out std_logic_vector(31 downto 0);
        
        mem_addr_m : out std_logic_vector(31 downto 0);
        read_data_m : in std_logic_vector(31 downto 0);
        
        mem_write_data_m : out std_logic_vector(31 downto 0);
        mem_write_enable_m : out std_logic;
        
        reg_write_enable_d, mem_write_enable_d, branch_d, jump_d : in std_logic;
        mem_to_reg_d, alu_source_d, reg_dest_d : in std_logic;
        alu_control_d : in std_logic_vector(2 downto 0);
        
        reg_s_hazard_e, reg_t_hazard_e : out std_logic_vector(4 downto 0);
        reg_write_hazard_m, reg_write_hazard_w : out std_logic;
        hazard_data_read_writeback_w, hazard_alu_out_writeback_m : out std_logic_vector(4 downto 0);
        hazard_forward_alu_a, hazard_forward_alu_b : in std_logic_vector(1 downto 0);
        
        stall_f, stall_d, flush_d, flush_e, flush_m : in std_logic;
        reg_s_hazard_d, reg_t_hazard_d : out std_logic_vector(4 downto 0);
        mem_to_reg_hazard_e, branch_hazard_m : out std_logic
      );
end component;
  component hazard_unit is
    port( reg_s_e, reg_t_e : in std_logic_vector(4 downto 0);
          reg_write_m, reg_write_w : in std_logic;
          hazard_alu_out_writeback_m, hazard_data_read_writeback_w : in std_logic_vector(4 downto 0);
          hazard_forward_alu_a, hazard_forward_alu_b : out std_logic_vector(1 downto 0);
        
          stall_f, stall_d, flush_d, flush_e, flush_m : out std_logic;
          reg_s_hazard_d, reg_t_hazard_d : in std_logic_vector(4 downto 0);
          mem_to_reg_hazard_e, branch_m, jump_d : in std_logic
          ); 
  end component;
  signal instruction_d : std_logic_vector(31 downto 0);
  
  signal reg_write_enable : std_logic;
  signal mem_write_enable_d : std_logic;
  signal branch : std_logic;
  signal jump : std_logic;
  
  signal r_instruction : std_logic;
  signal alu_control : std_logic_vector(2 downto 0);
  signal mem_to_reg : std_logic;
  
  signal reg_s_hazard_e, reg_t_hazard_e, hazard_data_read_writeback_w, hazard_alu_out_writeback_m : std_logic_vector(4 downto 0);
  signal reg_write_hazard_m, reg_write_hazard_w : std_logic;
  signal hazard_forward_alu_a, hazard_forward_alu_b : std_logic_vector(1 downto 0);
  
  signal stall_f, stall_d, flush_d, flush_e, flush_m : std_logic;
  signal reg_s_hazard_d, reg_t_hazard_d : std_logic_vector(4 downto 0);
  signal mem_to_reg_hazard_e, branch_hazard_m : std_logic;
begin
  CONTROL_INST : control_unit_pipe
    port map( opcode => instruction_d(31 downto 26),
              funct => instruction_d(5 downto 0),
              mem_write_enable => mem_write_enable_d,
              reg_write_enable => reg_write_enable,
              
              r_instruction => r_instruction,
              alu_control => alu_control,
              branch => branch,
              jump => jump,
              mem_to_reg => mem_to_reg
            );
  DATAPATH_INST : data_path_pipe
    port map( clk => clk,
              reset => reset,
              
              inst_addr_f => inst_addr,
              instruction_f => instruction,
              instruction_d_out => instruction_d,
              read_data_m => read_data,
              
              mem_addr_m => mem_addr,
              mem_write_data_m => mem_write_data,
              mem_write_enable_m => mem_write_enable,
              
              reg_write_enable_d => reg_write_enable,
              mem_write_enable_d => mem_write_enable_d,
              branch_d => branch,
              jump_d => jump,
              
              mem_to_reg_d => mem_to_reg,
              alu_source_d => not r_instruction,
              reg_dest_d => r_instruction,
              alu_control_d => alu_control,
              
              reg_s_hazard_e => reg_s_hazard_e,
              reg_t_hazard_e => reg_t_hazard_e,
              reg_write_hazard_m => reg_write_hazard_m,
              reg_write_hazard_w => reg_write_hazard_w,
              hazard_data_read_writeback_w => hazard_data_read_writeback_w,
              hazard_alu_out_writeback_m => hazard_alu_out_writeback_m,
              hazard_forward_alu_a => hazard_forward_alu_a,
              hazard_forward_alu_b => hazard_forward_alu_b,
              
              stall_f => stall_f,
              stall_d => stall_d,
              flush_d => flush_d,
              flush_e => flush_e,
              flush_m => flush_m,
              reg_s_hazard_d => reg_s_hazard_d,
              reg_t_hazard_d => reg_t_hazard_d,
              mem_to_reg_hazard_e => mem_to_reg_hazard_e,
              branch_hazard_m => branch_hazard_m
            );
  HAZARD_INST : hazard_unit
    port map( reg_s_e => reg_s_hazard_e,
              reg_t_e => reg_t_hazard_e,
              reg_write_m => reg_write_hazard_m,
              reg_write_w => reg_write_hazard_w,
              hazard_data_read_writeback_w => hazard_data_read_writeback_w,
              hazard_alu_out_writeback_m => hazard_alu_out_writeback_m,
              hazard_forward_alu_a => hazard_forward_alu_a,
              hazard_forward_alu_b => hazard_forward_alu_b,
              
              stall_f => stall_f,
              stall_d => stall_d,
              flush_d => flush_d,
              flush_e => flush_e,
              flush_m => flush_m,
              reg_s_hazard_d => reg_s_hazard_d,
              reg_t_hazard_d => reg_t_hazard_d,
              mem_to_reg_hazard_e => mem_to_reg_hazard_e,
              branch_m => branch_hazard_m,
              jump_d => jump
             );
  stall_instr <= stall_d;
  flush_instr <= flush_d;
end rtl;


-- Port 1 is read/write. Port 2 is read only.
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
use STD.TEXTIO.all;
entity sram_double is
  generic( ADDR_WIDTH : natural;
           WORD_WIDTH : natural := 32;
           FILENAME : string );
  port( clk : in std_logic;
        write_enable1 : in std_logic;
        addr1, addr2 : in std_logic_vector(ADDR_WIDTH-1 downto 0);
        write_data1 : in std_logic_vector(WORD_WIDTH-1 downto 0);
        read_data1, read_data2 : out std_logic_vector(WORD_WIDTH-1 downto 0);
        clear2, enable2 : in std_logic
       );       
end;
architecture rtl of sram_double is
    type ramtype is array((2 ** ADDR_WIDTH)-1 downto 0) of std_logic_vector(WORD_WIDTH-1 downto 0);
    impure function initialize return ramtype is
      file mem_file: TEXT;
      variable L: line;
      variable ch: character;
      variable i, index, result : integer;
      variable ret : ramtype;
    begin
      ret := (others => (others => '0'));
      index := 0;
      FILE_OPEN(mem_file, FILENAME, READ_MODE);
      while not endfile(mem_file) loop
        readline(mem_file, L);
        result := 0;
        for i in 1 to (WORD_WIDTH / 4) loop
          read(L, ch);
          if '0' <= ch and ch <= '9' then
            result := character'pos(ch) - character'pos('0');
          elsif 'a' <= ch and ch <= 'f' then
            result := character'pos(ch) - character'pos('a')+10;
          end if;
          ret(index)((WORD_WIDTH+3) - i*4 downto WORD_WIDTH-i*4) := to_std_logic_vector(result,4);
        end loop;
        index := index + 1;
      end loop;
      return ret;
    end function;
    signal mem : ramtype := initialize;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      if write_enable1 = '1' then
        mem(to_integer(addr1)) <= write_data1;
      end if;
      read_data1 <= mem(to_integer(addr1));
      if clear2 = '1' then
        read_data2 <= (others => '0');
      elsif enable2 = '1' then
        read_data2 <= mem(to_integer(addr2));
      end if;
    end if;
  end process;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
library work;
use work.cpu_basics.all;
entity mmap_double is
  port (clk, we1, reset, enable2, clear2 : in std_logic;
        a1, a2: in std_logic_vector(31 downto 0);
        wd1 : in std_logic_vector(31 downto 0);
        rd1, rd2: out std_logic_vector(31 downto 0);
        
        led : out std_logic_vector(3 downto 0)
        );
end;

-- Port 1 is read/write. Port 2 is readonly
architecture rtl of mmap_double is
  component sram_double is
    generic( ADDR_WIDTH : natural;
             WORD_WIDTH : natural := 32;
             FILENAME : string );
    port( clk : in std_logic;
          write_enable1 : in std_logic;
          addr1, addr2 : in std_logic_vector(ADDR_WIDTH-1 downto 0);
          write_data1 : in std_logic_vector(WORD_WIDTH-1 downto 0);
          read_data1, read_data2 : out std_logic_vector(WORD_WIDTH-1 downto 0);
          clear2, enable2 : in std_logic
        );       
  end component;  
  
  signal ram_read1, ram_read2 : std_logic_vector(31 downto 0);
  signal ram_write_enable : std_logic;
  signal led_write_enable : std_logic;
begin
  SRAM_INST : sram_double
    generic map( ADDR_WIDTH => 6,
                 FILENAME => "/home/vannatta/mipprog2.dat" )
    port map( clk => clk,
              clear2 => clear2,
              enable2 => enable2,
              write_enable1 => ram_write_enable,
              addr1 => a1(7 downto 2),
              addr2 => a2(7 downto 2),
              write_data1 => wd1,
              read_data1 => ram_read1,
              read_data2 => ram_read2 );
  process(all)
  begin
    if a1(7 downto 2) = "111111" then
      rd1 <= (others => '0');
      ram_write_enable <= '0';
      led_write_enable <= we1;
    else
      rd1 <= ram_read1;
      ram_write_enable <= we1;
      led_write_enable <= '0';
    end if;
    if a2(7 downto 2) = "111111" then
      rd2 <= (others => '0');
    else
      rd2 <= ram_read2;
    end if;
  end process;
  LED_INST : flopr
    generic map( WIDTH => 4 )
    port map( clk => clk,
              reset => reset,
              enable => led_write_enable,
              output => led,
              input => wd1(3 downto 0)
              );
end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
Library UNISIM;
use UNISIM.vcomponents.all;
entity top_pipe is
  port(clk, reset: in STD_LOGIC;
       led : out std_logic_vector(3 downto 0)
       );
end;
architecture rtl of top_pipe is
component cpu_pipe is
  port( clk : in std_logic;
        reset : in std_logic;

        inst_addr : out std_logic_vector(31 downto 0);
        instruction : in std_logic_vector(31 downto 0);
        
        mem_addr : out std_logic_vector(31 downto 0);
        read_data : in std_logic_vector(31 downto 0);
        
        mem_write_data : out std_logic_vector(31 downto 0);
        mem_write_enable : out std_logic;
        
        flush_instr, stall_instr : out std_logic
      );
end component;
component mmap_double is
  port (clk, we1, reset, enable2, clear2 : in std_logic;
        a1, a2: in std_logic_vector(31 downto 0);
        wd1 : in std_logic_vector(31 downto 0);
        rd1, rd2: out std_logic_vector(31 downto 0);
        
        led : out std_logic_vector(3 downto 0)
        );
end component;
component sync_deassert is
  port( clk, input : in std_logic;
        output : out std_logic
        );
end component;
component clk_wiz_0 is
  port( clk_out1 : out std_logic;
        locked : out std_logic;
        clk_in1 : in std_logic
        );
end component;
/*
module clk_wiz_0_clk_wiz 

 (// Clock in ports
  // Clock out ports
  output        clk_out1,
  // Status and control signals
  output        locked,
  input         clk_in1
 );
 */
  signal readdata, instruction: std_logic_vector(31 downto 0);
  signal reset_sync : std_logic;
  signal writedata, dataddr, instr_addr : STD_LOGIC_VECTOR(31 downto 0);
  signal memwrite : STD_LOGIC;
  
  -- FIXME I hate it.
  signal flush_instr, stall_instr : std_logic;
  signal fast_clk, fast_locked : std_logic;
begin

  PLL_INST : clk_wiz_0
    port map( clk_out1 => fast_clk,
              locked => fast_locked,
              clk_in1 => clk);

  DEASSERT_INST : sync_deassert
    port map( clk => clk,
              input => reset or not fast_locked,
              output => reset_sync
              );

  CPU1 : cpu_pipe
    port map(clk => clk,
             reset => reset_sync,
             
             inst_addr => instr_addr,
             instruction => instruction,
             
             mem_write_data => writedata,
             read_data => readdata,
             mem_addr => dataddr,
             mem_write_enable => memwrite,
             
             flush_instr => flush_instr,
             stall_instr => stall_instr
             );
  MEM_INST : mmap_double
    port map(clk => clk, we1 => memwrite, reset => reset_sync, enable2 => not stall_instr, clear2 => flush_instr, a1 => dataddr, wd1 => writedata, rd1 => readdata, a2 => instr_addr, rd2 => instruction, led => led);
             
end;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
use STD.TEXTIO.all;
entity srom_double is
  generic( ADDR_WIDTH : natural;
           WORD_WIDTH : natural := 32;
           FILENAME : string );
  port( clk, reset: in std_logic;
        addr1, addr2 : in std_logic_vector(ADDR_WIDTH-1 downto 0);
        read_data1, read_data2 : out std_logic_vector(WORD_WIDTH-1 downto 0)
       );       
end;
architecture rtl of srom_double is
    type romtype is array((2 ** ADDR_WIDTH)-1 downto 0) of std_logic_vector(WORD_WIDTH-1 downto 0);
    impure function initialize return romtype is
      file mem_file: TEXT;
      variable L: line;
      variable ch: character;
      variable i, index, result : integer;
      variable ret : romtype;
    begin
      ret := (others => (others => '0'));
      index := 0;
      FILE_OPEN(mem_file, FILENAME, READ_MODE);
      while not endfile(mem_file) loop
        readline(mem_file, L);
        result := 0;
        for i in 1 to (WORD_WIDTH / 4) loop
          read(L, ch);
          if '0' <= ch and ch <= '9' then
            result := character'pos(ch) - character'pos('0');
          elsif 'a' <= ch and ch <= 'f' then
            result := character'pos(ch) - character'pos('a')+10;
          end if;
          ret(index)((WORD_WIDTH+3) - i*4 downto WORD_WIDTH-i*4) := to_std_logic_vector(result,4);
        end loop;
        index := index + 1;
      end loop;
      return ret;
    end function;
    constant mem : romtype := initialize;
begin
  process(clk)
  begin
    if falling_edge(clk) then
      read_data1 <= mem(to_integer(addr1));
      read_data2 <= mem(to_integer(addr2));
    end if;
  end process;
end rtl;