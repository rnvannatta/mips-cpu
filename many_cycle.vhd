library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
library work;
use work.cpu_basics.all;
entity data_path_ms is
  port( clk : in std_logic; -- clock signal
        reset : in std_logic; -- reset program counter
        opcode : out std_logic_vector(5 downto 0);
        funct : out std_logic_vector(5 downto 0);
        
        write_data : out std_logic_vector(31 downto 0); -- data to be written to memory
        read_data : in std_logic_vector(31 downto 0); -- data to be read from memory
        data_addr : out std_logic_vector(31 downto 0); -- data address to write to or read from
        
        -- Control flags
        pc_write : in std_logic; -- whether or not to write to the pc
        instr_or_data : in std_logic; -- whether to load an instruction (from the pc) or data (from the alu)
        instr_reg_write : in std_logic; -- whether to update the instruction register
        reg_write_enable : in std_logic; -- whether to write to the register file
        r_instruction : in std_logic; -- whether or not instruction is r type, changes input to w of register file and wd of register file
        alu_control : in std_logic_vector(2 downto 0); -- control what operation the ALU performs
        
        alu_a_arg : in std_logic; -- whether to load the pc or argument 1 from register file into the alu
        alu_b_arg : in std_logic_vector(1 downto 0); -- whether to load (00) argument 2, (01) 4 for pc increment, (10), the immediate, or (11) the word shifted immediate into the alu 
    
        
        branch : in std_logic; -- whether to check the ALU for branching, otherwise check the pc_write flag
        pc_source : in std_logic_vector(1 downto 0); -- whether to use (00) the immediate ALU result, (01) the registered ALU result, or (10) the jump addr to feed the PC
        mem_to_reg : in std_logic -- whether to write memory or the alu result to register
      );
end data_path_ms;

architecture rtl of data_path_ms is
  component alu is
    port( control : in std_logic_vector(2 downto 0);
          a : in std_logic_vector(31 downto 0);
          b : in std_logic_vector(31 downto 0);
          c : out std_logic_vector(31 downto 0);
          z : out std_logic
        );
  end component alu;
  
  signal pc_output : std_logic_vector(31 downto 0); -- output from pc register
  signal jmp_addr : std_logic_vector(31 downto 0); -- address jump is jumping to
  signal pc_input : std_logic_vector(31 downto 0); -- input into pc register
  signal instruction : std_logic_vector(31 downto 0); -- instruction read from memory
  signal data : std_logic_Vector(31 downto 0); -- data read from memory
  signal write_register : std_logic_vector(4 downto 0); -- which register to write to
  signal reg_write_data : std_logic_Vector(31 downto 0); -- data to write to register file
  signal a1_out : std_logic_vector(31 downto 0); -- output fed into a1 register
  signal a1 : std_logic_vector(31 downto 0); -- first argument read from register file
  signal a2_out : std_logic_vector(31 downto 0); -- output fed into a2 register
  signal a2 : std_logic_vector(31 downto 0); -- second argument read from register file
  signal ext_imm : std_logic_vector(31 downto 0); -- sign extended immediate
  signal shift_imm : std_logic_vector(31 downto 0); -- word shifted sign extended immediate
  signal alu_input_a : std_logic_vector(31 downto 0); -- first argument to alu
  signal alu_input_b : std_logic_vector(31 downto 0); -- second argument to alu
  signal alu_res_out : std_logic_vector(31 downto 0); -- output fed into alu_res register, without register backing
  signal alu_res : std_logic_vector(31 downto 0); -- result of alu computation
  signal zero_flag : std_logic; -- zero flag of ALU
begin
  opcode <= instruction(31 downto 26);
  funct <= instruction(5 downto 0);
  write_data <= a2;
  
  PC_MUX : qmux
    generic map(width => 32)
    port map( a => alu_res_out,
              b => alu_res,
              c => jmp_addr,
              d => (others => '1'),
              output => pc_input,
              y => pc_source
              );

  PC_REGISTER : flopr
    port map( clk => clk,
              reset => reset,
              enable => (pc_write or (zero_flag and branch)),
              output => pc_output,
              input => pc_input );
  
  JMP_INST : jump_address
     port map( output => jmp_addr,
               imm => instruction(25 downto 0),
               pc_hi => pc_output(31 downto 28)
               );
  
  INSTR_OR_DATA_MUX : bimux
    generic map(width => 32)
    port map( a => pc_output,
              b => alu_res,
              output => data_addr,
              y => instr_or_data
              );
              
  INSTRUCTION_REGISTER : flopr
    port map( clk => clk,
              reset => '0',
              enable => instr_reg_write,
              output => instruction,
              input => read_data );
  
  DATA_REGISTER : flopr
    port map( clk => clk,
              reset => '0',
              output => data,
              input => read_data
              );
  
  REG_WRITE_MUX : bimux
    generic map( width => 5 )
    port map( a => instruction(20 downto 16),
              b => instruction(15 downto 11),
              y => r_instruction,
              output => write_register
            );
  REG_WRITE_DATA_MUX : bimux
    generic map( width => 32)
    port map( a => alu_res,
              b => data,
              y => mem_to_reg,
              output => reg_write_data
              );
  
  REGISTER_FILE_INST : register_file
    port map( clk => clk,
              a1 => instruction(25 downto 21),
              a2 => instruction(20 downto 16),
              w => write_register,
              write_data => reg_write_data,
              write_enable => reg_write_enable,
              rd1 => a1_out,
              rd2 => a2_out
              );
  
  A1_REGISTER : flopr
    port map( clk => clk,
              reset => '0',
              output => a1,
              input => a1_out );
              
  B1_REGISTER : flopr
    port map( clk => clk,
              reset => '0',
              output => a2,
              input => a2_out );
              
  IMM_EXTEND : sign_extend
    port map ( input => instruction(15 downto 0),
               output => ext_imm );
               
  IMM_SHIFT : shift_word_to_mem
    port map ( input => ext_imm,
               output => shift_imm );
  
  ALU_A_BIMUX : bimux
    generic map(width => 32)
    port map( a => pc_output,
              b => a1,
              output => alu_input_a,
              y => alu_a_arg
            );
  
  ALU_B_QMUX : qmux
    generic map(width => 32)
    port map( a =>  a2,
              b => (3 downto 0 => X"4", others => '0'),
              c => ext_imm,
              d => shift_imm,
              output => alu_input_b,
              y => alu_b_arg
            );  
     
  ALU_INST : alu
    port map( control => alu_control,
              a => alu_input_a,
              b => alu_input_b,
              c => alu_res_out,
              z => zero_flag
              );
              
              
  ALU_RES_REGISTER : flopr
    port map( clk => clk,
              reset => '0',
              output => alu_res,
              input => alu_res_out
              );
              
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity main_decode_ms is
  port( clk : in std_logic;
        reset : in std_logic;
        opcode : in std_logic_vector(5 downto 0);
        
        -- Output Control Signals
        pc_write : out std_logic; -- whether or not to write to the pc
        instr_or_data : out std_logic; -- whether to load an instruction (from the pc) or data (from the alu)
        instr_reg_write : out std_logic; -- whether to update the instruction register
        reg_write_enable : out std_logic; -- whether to write to the register file
        mem_write_enable : out std_logic; -- whether or not to write to memory
        r_instruction : out std_logic; -- whether or not instruction is r type, changes input to w of register file and wd of register file
        alu_op : out std_logic_vector(1 downto 0); -- control what operation the ALU performs, or whether it uses the funct input
      
        alu_a_arg : out std_logic; -- whether to load the pc or argument 1 from register file into the alu
        alu_b_arg : out std_logic_vector(1 downto 0); -- whether to load (00) argument 2, (01) 4 for pc increment, (10), the immediate, or (11) the shifted branch immediate into the alu 
  
      
        branch : out std_logic; -- whether to check the ALU for branching, otherwise check the pc_write flag
        pc_source : out std_logic_vector(1 downto 0); -- whether to use (00) the immediate ALU result, (01) the registered ALU result, or (10) the jump addr to feed the PC
        mem_to_reg : out std_logic -- whether to write memory or the alu result to register
        );
end main_decode_ms;
architecture rtl of main_decode_ms is
  type statetype is (INVALID_STATE, FETCH, DECODE, MEM_ADDR_COMP, MEM_ADDR_READ, MEM_WRITEBACK, MEM_WRITE, EXECUTE, ALU_WRITEBACK, BRANCH_STATE, JUMP_STATE, IMM_EXECUTE, IMM_ALU_WRITEBACK);
  signal state : statetype := FETCH;
  signal next_state : statetype;
begin
  process(clk, reset) begin
    if reset = '1' then
      state <= FETCH;
    elsif rising_edge(clk) then
      state <= next_state;
    end if;
  end process;
  
  process(all) begin
    case state is
      when FETCH =>
        pc_write <= '1';
        instr_or_data <= '0';
        instr_reg_write <= '1';
        reg_write_enable <= '0';
        mem_write_enable <= '0';
        r_instruction <= '-';
        alu_op <= "00";
        alu_a_arg <= '0';
        alu_b_arg <= "01";
        branch <= '0';
        pc_source <= "00";
        mem_to_reg <= '-';
        
        next_state <= DECODE;
      when DECODE =>
        pc_write <= '0';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '0';
        mem_write_enable <= '0';
        r_instruction <= '-';
        alu_op <= "00";
        alu_a_arg <= '0';
        alu_b_arg <= "11";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '-';
        case opcode is
          when "000000" => -- r-type
            next_state <= EXECUTE;
          when "100011" | "101011" => -- lw | sw
            next_state <= MEM_ADDR_COMP;
          when "000100" => -- beq
            next_state <= BRANCH_STATE;
          when "000010" => -- jmp
            next_state <= JUMP_STATE;
          when "001000" => -- addi
            next_state <= IMM_EXECUTE;
          when others =>
            next_state <= INVALID_STATE;
        end case;
      when MEM_ADDR_COMP =>
        pc_write <= '0';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '0';
        mem_write_enable <= '0';
        r_instruction <= '-';
        alu_op <= "00";
        alu_a_arg <= '1';
        alu_b_arg <= "10";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '-';
        
        case opcode is
          when "100011" => -- lw
            next_state <= MEM_ADDR_READ;
          when "101011" => -- sw
            next_state <= MEM_WRITE;
          when others =>
            next_state <= INVALID_STATE;
        end case;
      when MEM_ADDR_READ =>
        pc_write <= '0';
        instr_or_data <= '1';
        instr_reg_write <= '0';
        reg_write_enable <= '0';
        mem_write_enable <= '0';
        r_instruction <= '-';
        alu_op <= "--";
        alu_a_arg <= '-';
        alu_b_arg <= "--";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '-';
        
        next_state <= MEM_WRITEBACK;
      when MEM_WRITEBACK =>
        pc_write <= '0';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '1';
        mem_write_enable <= '0';
        r_instruction <= '0';
        alu_op <= "--";
        alu_a_arg <= '-';
        alu_b_arg <= "--";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '1';
        
        next_state <= FETCH;
      when MEM_WRITE =>
        pc_write <= '0';
        instr_or_data <= '1';
        instr_reg_write <= '0';
        reg_write_enable <= '0';
        mem_write_enable <= '1';
        r_instruction <= '-';
        alu_op <= "--";
        alu_a_arg <= '-';
        alu_b_arg <= "--";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '-';
        
        next_state <= FETCH;
      when EXECUTE =>
        pc_write <= '0';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '0';
        mem_write_enable <= '0';
        r_instruction <= '-';
        alu_op <= "10";
        alu_a_arg <= '1';
        alu_b_arg <= "00";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '-';
        
        next_state <= ALU_WRITEBACK;
      when ALU_WRITEBACK =>
        pc_write <= '0';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '1';
        mem_write_enable <= '0';
        r_instruction <= '1';
        alu_op <= "--";
        alu_a_arg <= '-';
        alu_b_arg <= "--";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '0';
        
        next_state <= FETCH;
      when IMM_EXECUTE =>
        pc_write <= '0';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '0';
        mem_write_enable <= '0';
        r_instruction <= '-';
        alu_op <= "00";
        alu_a_arg <= '1';
        alu_b_arg <= "10";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '-';
        
        next_state <= IMM_ALU_WRITEBACK;
      when IMM_ALU_WRITEBACK =>
        pc_write <= '0';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '1';
        mem_write_enable <= '0';
        r_instruction <= '0';
        alu_op <= "--";
        alu_a_arg <= '-';
        alu_b_arg <= "--";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '0';
        
        next_state <= FETCH;
      when BRANCH_STATE =>
        pc_write <= '0';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '0';
        mem_write_enable <= '0';
        r_instruction <= '-';
        alu_op <= "01";
        alu_a_arg <= '1';
        alu_b_arg <= "00";
        branch <= '1';
        pc_source <= "01";
        mem_to_reg <= '-';
        
        next_state <= FETCH;
      when JUMP_STATE =>
        pc_write <= '1';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '0';
        mem_write_enable <= '0';
        r_instruction <= '-';
        alu_op <= "--";
        alu_a_arg <= '-';
        alu_b_arg <= "--";
        branch <= '0';
        pc_source <= "10";
        mem_to_reg <= '-';
        
        next_state <= FETCH;
      when others =>
        pc_write <= '0';
        instr_or_data <= '-';
        instr_reg_write <= '0';
        reg_write_enable <= '0';
        mem_write_enable <= '0';
        r_instruction <= '-';
        alu_op <= "--";
        alu_a_arg <= '-';
        alu_b_arg <= "--";
        branch <= '0';
        pc_source <= "--";
        mem_to_reg <= '-';
        
        next_state <= INVALID_STATE;
    end case;
  end process;
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity control_unit_ms is
  port( clk : in std_logic;
        reset : in std_logic;
        opcode : in std_logic_vector(5 downto 0);
        funct : in std_logic_vector(5 downto 0);
        
        -- Output Control Signals
        pc_write : out std_logic; -- whether or not to write to the pc
        instr_or_data : out std_logic; -- whether to load an instruction (from the pc) or data (from the alu)
        instr_reg_write : out std_logic; -- whether to update the instruction register
        reg_write_enable : out std_logic; -- whether to write to the register file
        mem_write_enable : out std_logic; -- whether or not to write to memory
        r_instruction : out std_logic; -- whether or not instruction is r type, changes input to w of register file and wd of register file
        alu_control : out std_logic_vector(2 downto 0); -- control what operation the ALU performs
        
        alu_a_arg : out std_logic; -- whether to load the pc or argument 1 from register file into the alu
        alu_b_arg : out std_logic_vector(1 downto 0); -- whether to load (00) argument 2, (01) 4 for pc increment, (10), the immediate, or (11) the shifted branch immediate into the alu
    
        
        branch : out std_logic; -- whether to check the ALU for branching, otherwise check the pc_write flag
        pc_source : out std_logic_vector(1 downto 0); -- whether to use (00) the immediate ALU result, (01) the registered ALU result, or (10) the jump addr to feed the PC
        mem_to_reg : out std_logic
      );
end control_unit_ms;

architecture rtl of control_unit_ms is
  component main_decode_ms is
    port( clk : in std_logic;
          reset : in std_logic;
          opcode : in std_logic_vector(5 downto 0);
          
          -- Output Control Signals
          pc_write : out std_logic; -- whether or not to write to the pc
          instr_or_data : out std_logic; -- whether to load an instruction (from the pc) or data (from the alu)
          instr_reg_write : out std_logic; -- whether to update the instruction register
          reg_write_enable : out std_logic; -- whether to write to the register file
          mem_write_enable : out std_logic; -- whether or not to write to memory
          r_instruction : out std_logic; -- whether or not instruction is r type, changes input to w of register file and wd of register file
          alu_op : out std_logic_vector(1 downto 0); -- control what operation the ALU performs, or whether it uses the funct input
        
          alu_a_arg : out std_logic; -- whether to load the pc or argument 1 from register file into the alu
          alu_b_arg : out std_logic_vector(1 downto 0); -- whether to load (00) ??, (01) 4 for pc increment, (10), ??, or (11) ?? into the alu 
    
        
          branch : out std_logic; -- whether to check the ALU for branching, otherwise check the pc_write flag
          pc_source : out std_logic_vector(1 downto 0); -- whether to use (00) the immediate ALU result, (01) the registered ALU result, or (10) the jump addr to feed the PC
          mem_to_reg : out std_logic -- whether to write memory or the alu result to register
          );
  end component main_decode_ms;
  
  component alu_decode is
    port( funct : in std_logic_vector(5 downto 0);
          alu_op : in std_logic_vector(1 downto 0);
          alu_control : out std_logic_vector(2 downto 0)
        );
  end component alu_decode;
  
  signal alu_op : std_logic_vector(1 downto 0);
begin
  MAIN_INST : main_decode_ms
    port map( clk => clk,
              reset => reset,
              opcode => opcode,
              
              pc_write => pc_write,
              instr_or_data => instr_or_data,
              instr_reg_write => instr_reg_write,
              reg_write_enable => reg_write_enable,
              mem_write_enable => mem_write_enable,
              r_instruction => r_instruction,
              alu_op => alu_op,
              alu_a_arg => alu_a_arg,
              alu_b_arg => alu_b_arg,
              
              branch => branch,
              pc_source => pc_source,
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
entity cpu_mc is
  port( clk : in std_logic;
        reset : in std_logic;
        write_data : out std_logic_vector(31 downto 0); -- data to be written to memory
        read_data : in std_logic_vector(31 downto 0); -- data to be read from memory
        data_addr : out std_logic_vector(31 downto 0); -- data address to write to or read from
        mem_write_enable : out std_logic
        
      );
end cpu_mc;
architecture rtl of cpu_mc is
component control_unit_ms is
  port( clk : in std_logic;
        reset : in std_logic;
        opcode : in std_logic_vector(5 downto 0);
        funct : in std_logic_vector(5 downto 0);
        
        -- Output Control Signals
        pc_write : out std_logic; -- whether or not to write to the pc
        instr_or_data : out std_logic; -- whether to load an instruction (from the pc) or data (from the alu)
        instr_reg_write : out std_logic; -- whether to update the instruction register
        reg_write_enable : out std_logic; -- whether to write to the register file
        mem_write_enable : out std_logic; -- whether or not to write to memory
        r_instruction : out std_logic; -- whether or not instruction is r type, changes input to w of register file and wd of register file
        alu_control : out std_logic_vector(2 downto 0); -- control what operation the ALU performs
        
        alu_a_arg : out std_logic; -- whether to load the pc or argument 1 from register file into the alu
        alu_b_arg : out std_logic_vector(1 downto 0); -- whether to load (00) argument 2, (01) 4 for pc increment, (10), the immediate, or (11) the shifted branch immediate into the alu
    
        
        branch : out std_logic; -- whether to check the ALU for branching, otherwise check the pc_write flag
        pc_source : out std_logic_vector(1 downto 0); -- whether to use (00) the immediate ALU result, (01) the registered ALU result, or (10) the jump addr to feed the PC
        mem_to_reg : out std_logic
      );
end component control_unit_ms;
component data_path_ms is
  port( clk : in std_logic; -- clock signal
        reset : in std_logic; -- reset program counter
        opcode : out std_logic_vector(5 downto 0);
        funct : out std_logic_vector(5 downto 0);
        
        write_data : out std_logic_vector(31 downto 0); -- data to be written to memory
        read_data : in std_logic_vector(31 downto 0); -- data to be read from memory
        data_addr : out std_logic_vector(31 downto 0); -- data address to write to or read from
        
        -- Control flags
        pc_write : in std_logic; -- whether or not to write to the pc
        instr_or_data : in std_logic; -- whether to load an instruction (from the pc) or data (from the alu)
        instr_reg_write : in std_logic; -- whether to update the instruction register
        reg_write_enable : in std_logic;  -- whether to write to the register file
        r_instruction : in std_logic; -- whether or not instruction is r type, changes input to w of register file and wd of register file
        alu_control : in std_logic_vector(2 downto 0); -- control what operation the ALU performs
        
        alu_a_arg : in std_logic; -- whether to load the pc or argument 1 from register file into the alu
        alu_b_arg : in std_logic_vector(1 downto 0); -- whether to load (00) argument 2, (01) 4 for pc increment, (10), the immediate, or (11) the word shifted immediate into the alu 
    
        branch : in std_logic; -- whether to check the ALU for branching, otherwise check the pc_write flag
        pc_source : in std_logic_vector(1 downto 0); -- whether to use (00) the immediate ALU result, (01) the registered ALU result, or (10) the jump addr to feed the PC
        mem_to_reg : in std_logic -- whether to write memory or the alu result to register
      );
end component data_path_ms;
  signal opcode, funct : std_logic_vector(5 downto 0);
  signal pc_write : std_logic; -- whether or not to write to the pc
  signal instr_or_data : std_logic; -- whether to load an instruction (from the pc) or data (from the alu)
  signal instr_reg_write : std_logic; -- whether to update the instruction register
  signal reg_write_enable : std_logic; -- whether to write to the register file
  signal r_instruction : std_logic; -- whether or not instruction is r type, changes input to w of register file and wd of register file
  signal alu_control : std_logic_vector(2 downto 0); -- control what operation the ALU performs
        
  signal alu_a_arg : std_logic; -- whether to load the pc or argument 1 from register file into the alu
  signal alu_b_arg : std_logic_vector(1 downto 0); -- whether to load (00) argument 2, (01) 4 for pc increment, (10), the immediate, or (11) the word shifted immediate into the alu 
    
        
  signal branch : std_logic; -- whether to check the ALU for branching, otherwise check the pc_write flag
  signal pc_source : std_logic_vector(1 downto 0); -- whether to use (00) the immediate ALU result, (01) the registered ALU result, or (10) the jump addr to feed the PC
  signal mem_to_reg : std_logic; -- whether to write memory or the alu result to register
begin
  CONTROL_INST : control_unit_ms
    port map( clk => clk,
              reset => reset,
              opcode => opcode,
              funct => funct,
              
              pc_write => pc_write,
              instr_or_data => instr_or_data,
              instr_reg_write => instr_reg_write,
              reg_write_enable => reg_write_enable,
              mem_write_enable => mem_write_enable,
              r_instruction => r_instruction,
              alu_control => alu_control,
              alu_a_arg => alu_a_arg,
              alu_b_arg => alu_b_arg,
              branch => branch,
              pc_source => pc_source,
              mem_to_reg => mem_to_reg
              
              );
  DATA_INST : data_path_ms
    port map( clk => clk,
              reset => reset,
              opcode => opcode,
              funct => funct,
              write_data => write_data,
              read_data => read_data,
              data_addr => data_addr,
              
              pc_write => pc_write,
              instr_or_data => instr_or_data,
              instr_reg_write => instr_reg_write,
              reg_write_enable => reg_write_enable,
              r_instruction => r_instruction,
              alu_control => alu_control,
              alu_a_arg => alu_a_arg,
              alu_b_arg => alu_b_arg,
              branch => branch,
              pc_source => pc_source,
              mem_to_reg => mem_to_reg
              );
end rtl;

-- FIXME generated distributed memory: need to clock the output
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity sram_single is
  generic( ADDR_WIDTH : natural;
           WORD_WIDTH : natural := 32 );
  port( clk : in std_logic;
        write_enable : in std_logic;
        addr : in std_logic_vector(ADDR_WIDTH-1 downto 0);
        write_data : in std_logic_vector(WORD_WIDTH-1 downto 0);
        read_data : out std_logic_vector(WORD_WIDTH-1 downto 0)
       );       
end entity sram_single;
architecture rtl of sram_single is
    type ramtype is array((2 ** ADDR_WIDTH)-1 downto 0) of std_logic_vector(WORD_WIDTH-1 downto 0);
    signal mem : ramtype;
begin
  process(clk)
  begin
    if rising_edge(clk) then
      if write_enable = '1' then
        mem(to_integer(addr)) <= write_data;
      end if;
    end if;
  end process;
  read_data <= mem(to_integer(addr));
end rtl;

-- FIXME generated LUTs: need to clock the output
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
use STD.TEXTIO.all;
entity srom_single is
  generic( ADDR_WIDTH : natural;
           WORD_WIDTH : natural := 32;
           FILENAME : string );
  port( addr : in std_logic_vector(ADDR_WIDTH-1 downto 0);
        read_data : out std_logic_vector(WORD_WIDTH-1 downto 0)
       );       
end entity srom_single;
architecture rtl of srom_single is
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
  read_data <= mem(to_integer(addr));
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD_UNSIGNED.ALL;
entity mmap is
  port (clk, we, reset : in std_logic;
        a: in std_logic_vector(31 downto 0);
        wd : in std_logic_vector(31 downto 0);
        rd: out std_logic_vector(31 downto 0);
        
        led : out std_logic_vector(3 downto 0)
        );
end entity mmap;

architecture rtl of mmap is
  component flopr is
    generic ( WIDTH : natural := 32 );
    port( clk : in std_logic;
          reset : in std_logic := '0';
          enable : in std_logic := '1';
          output : out std_logic_vector(WIDTH-1 downto 0);
          input : in std_logic_vector(WIDTH-1 downto 0)
        );
  end component flopr;
  
  component srom_single is
    generic( ADDR_WIDTH : natural;
             WORD_WIDTH : natural := 32;
             FILENAME : string );
    port( addr : in std_logic_vector(ADDR_WIDTH-1 downto 0);
          read_data : out std_logic_vector(WORD_WIDTH-1 downto 0)
         );       
  end component srom_single;
  component sram_single is
    generic( ADDR_WIDTH : natural;
             WORD_WIDTH : natural := 32 );
    port( clk : in std_logic;
          write_enable : in std_logic;
          addr : in std_logic_vector(ADDR_WIDTH-1 downto 0);
          write_data : in std_logic_vector(WORD_WIDTH-1 downto 0);
          read_data : out std_logic_vector(WORD_WIDTH-1 downto 0)
        );
  end component;  
  
  signal rom_read : std_logic_vector(31 downto 0);
  signal ram_read : std_logic_vector(31 downto 0);
  signal ram_write_enable : std_logic;
  signal led_write_enable : std_logic;
begin
  SROM_INST : srom_single
    generic map( ADDR_WIDTH => 5,
                 WORD_WIDTH => 32,
                 FILENAME => "/home/vannatta/mipprog2.dat" )
    port map( addr => a(6 downto 2),
              read_data => rom_read );
              
  SRAM_INST : sram_single
    generic map( ADDR_WIDTH => 5 )
    port map( clk => clk,
              write_enable => ram_write_enable,
              addr => a(6 downto 2),
              write_data => wd,
              read_data => ram_read );
  process(all)
  begin
    if a(7 downto 2) = "111111" then
      rd <= (others => '0');
      ram_write_enable <= '0';
      led_write_enable <= we;
    elsif a(7) = '0' then
      rd <= rom_read;
      ram_write_enable <= '0';
      led_write_enable <= '0';
    else
      rd <= ram_read;
      ram_write_enable <= we;
      led_write_enable <= '0';
    end if;
  end process;
  LED_INST : flopr
    generic map( WIDTH => 4 )
    port map( clk => clk,
              reset => reset,
              enable => led_write_enable,
              output => led,
              input => wd(3 downto 0)
              );
end architecture;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity sync_deassert is
  port( clk, input : in std_logic;
        output : out std_logic
        );
end entity;
architecture rtl of sync_deassert is
  component flopr is
    generic ( WIDTH : natural := 32 );
    port( clk : in std_logic;
          reset : in std_logic := '0';
          enable : in std_logic := '1';
          output : out std_logic_vector(WIDTH-1 downto 0);
          input : in std_logic_vector(WIDTH-1 downto 0)
        );
  end component flopr;
  signal middle : std_logic_vector(0 downto 0);
  signal inv_output : std_logic;
begin
  DELAY1 : flopr
    generic map( WIDTH => 1 )
    port map (clk => clk, reset => input, output => middle, input => (0 => not input) );
  DELAY2 : flopr
    generic map( WIDTH => 1 )
    port map (clk => clk, reset => input, output(0) => inv_output, input => middle );
  output <= not inv_output;
end;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
entity top_mc is
  port(clk, reset: in STD_LOGIC;
       led : out std_logic_vector(3 downto 0)
       );
end;
architecture rtl of top_mc is
component cpu_mc is
  port( clk : in std_logic;
        reset : in std_logic;
        write_data : out std_logic_vector(31 downto 0); -- data to be written to memory
        read_data : in std_logic_vector(31 downto 0); -- data to be read from memory
        data_addr : out std_logic_vector(31 downto 0); -- data address to write to or read from
        mem_write_enable : out std_logic
      );
end component;
component mmap is
  port (clk, we, reset : in std_logic;
        a: in std_logic_vector(31 downto 0);
        wd : in std_logic_vector(31 downto 0);
        rd: out std_logic_vector(31 downto 0);
        
        led : out std_logic_vector(3 downto 0)
        
        );
end component mmap;
component sync_deassert is
  port( clk, input : in std_logic;
        output : out std_logic
        );
end component;
  signal readdata: std_logic_vector(31 downto 0);
  signal reset_sync : std_logic;
  signal writedata, dataddr : STD_LOGIC_VECTOR(31 downto 0);
  signal memwrite : STD_LOGIC;
begin

  DEASSERT_INST : sync_deassert
    port map( clk => clk,
              input => reset,
              output => reset_sync
              );

  CPU1 : cpu_mc
    port map(clk => clk,
             reset => reset_sync,
             write_data => writedata,
             read_data => readdata,
             data_addr => dataddr,
             mem_write_enable => memwrite
             );
  MEM_INST : mmap
    port map(clk => clk, we => memwrite, reset => reset_sync, a => dataddr, wd => writedata, rd => readdata, led => led);
             
end;