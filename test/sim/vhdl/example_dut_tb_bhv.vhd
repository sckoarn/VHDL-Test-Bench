-- this file generated from template by ttb_gen

architecture bhv of example_dut_tb is


  
  signal tb_clk            : std_logic;
  
-------------------------------------------------------------------------
-- Component defintion

-------------------------------------------------------------------------------
-- USER Component instantiations
--for all: Arbitor      use entity tb_objects.arbitor(bhv);


  begin

-------------------------------------------------------------------------------
-- clock driver process
-- the main clock generator
clock_driver:
  process
  begin
    tb_clk <=  '0';
    wait for 5 ns;
    tb_clk <=  '1';
    wait for 5 ns;
  end process clock_driver;

  
  ex_clk_in  <=  tb_clk;
  
  --------------------------------------------------------------------------
  -- Read_file Process:
  --
  -- This process is the main process of the testbench.  This process reads
  -- the stumulus file, parses it, creates lists of records, then uses these
  -- lists to exicute user instructions.  There are two passes through the
  -- script.  Pass one reads in the stimulus text file, checks it, creates
  -- lists of valid instructions, valid list of variables and finialy a list
  -- of user instructions(the sequence).  The second pass through the file,
  -- records are drawn from the user instruction list, variables are converted
  -- to integers and put through the elsif structure for execution.
  --------------------------------------------------------------------------
  Read_file: process
    variable inst_list    : inst_def_ptr;  -- the instruction list
    variable defined_vars : var_field_ptr; -- defined variables
    variable inst_sequ    : stim_line_ptr; -- the instruction sequence
    variable file_list    : file_def_ptr;  -- pointer to the list of file names
    variable last_sequ_num: integer;
    variable last_sequ_ptr: stim_line_ptr;

    variable inst_group   : integer;      --  the instruction group
    variable inst_idx     : integer;      --  the instruction index
    variable instruction  : text_field;   -- instruction field
    variable par1         : integer;      -- paramiter 1
    variable par2         : integer;      -- paramiter 2
    variable par3         : integer;      -- paramiter 3
    variable par4         : integer;      -- paramiter 4
    variable par5         : integer;      -- paramiter 5
    variable par6         : integer;      -- paramiter 6
    variable txt          : stm_text_ptr;
    variable nbase        : base;         -- the number base to use
    variable len          : integer;      -- length of the instruction field
    variable file_line    : integer;      -- Line number in the stimulus file
    variable file_name    : text_line;    -- the file name the line came from
    variable v_line       : integer := 0; -- sequence number
    variable stack        : stack_register; -- Call stack
    variable stack_ptr    : integer  := 0;  -- call stack pointer
    variable wh_stack     : stack_register; -- while stack
    variable wh_dpth      : integer := 0;   -- while depth
    variable wh_ptr       : integer  := 0;  -- while pointer
    variable loop_num         : integer := 0;
    variable curr_loop_count  : int_array := (others => 0);
    variable term_loop_count  : int_array := (others => 0);
    variable loop_line        : int_array := (others => 0);

    variable messages     : boolean  := false;
    variable if_state     : boolean  := FALSE;
    variable wh_state     : boolean  := FALSE;
    variable wh_end       : boolean  := FALSE;
    variable rand         : std_logic_vector(31 downto 0);
    variable rand_back    : std_logic_vector(31 downto 0);
    
    variable temp_int     : integer;
    variable temp_index   : integer;
    variable temp_str     : text_field;
    variable valid        : integer;
    variable v_temp_vec1  : std_logic_vector(31 downto 0);
    variable v_temp_vec2  : std_logic_vector(31 downto 0);

    variable v_read_data  : std_logic_vector(31 downto 0);
    --------------------------------------------------------------------------
    --  Area for Procedures which may be usefull to more than one instruction.
    --    By coding here commonly used  code sections ...
    --    you know the benifits.
    ---------------------------------------------------------------------
    -----------------------------------------------------------------
    -- This procedure writes to the arbitor model access port
--    procedure arb_write(add: in integer; .....
--    end arb_write;

    
  begin  -- process Read_file

    ex_reset_n <=  '1';
--    ex_clk_in  <=  '0';
    stm_add    <=  (others => 'Z');
    stm_dat    <=  (others => 'Z');
    stm_rwn    <=  '1';
    stm_req_n  <=  '1';

    -----------------------------------------------------------------------
    --  Stimulus file instruction definition
    --    see documentaion for more details on instruction groups
    ------------------------------------------------------------------------
    --  test bench default instructions
    tb_defaults(inst_list);
    --  User defined instructions
    define_instruction(inst_list, "RESET_SYS", 0);
    define_instruction(inst_list, "READ_PINS", 1);
    define_instruction(inst_list, "READ_VAR", 1);
    define_instruction(inst_list, "WRITE_DUT", 2);
    define_instruction(inst_list, "VERIFY", 1);
    define_instruction(inst_list, "SHOW_BIN", 0);
    define_instruction(inst_list, "SHOW_DEC", 0);
    define_instruction(inst_list, "SHOW_HEX", 0);

    --dump_insts(inst_list);
    ------------------------------------------------------------------------
    -- Read, test, and load the stimulus file
    read_instruction_file(stimulus_file, inst_list, defined_vars, inst_sequ, 
                          file_list);
    
    -- initialize last info
    last_sequ_num  := 0;
    last_sequ_ptr  := inst_sequ;

--    dump_stm(inst_sequ);
    list_stm(inst_sequ);
------------------------------------------------------------------------
-- Using the Instruction record list, get the instruction and implement
-- it as per the statements in the elsif tree.
  while(v_line < inst_sequ.num_of_lines) loop
    v_line := v_line + 1;
    report "Accessing line: " & integer'image(v_line);
    access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
         inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
         file_name, file_line, last_sequ_num, last_sequ_ptr);
         
    --dump_current(v_line, inst_group,inst_idx, instruction,
    --             par1, par2, par3, par4, par5, par6, txt, len,
    --             file_name, file_line, last_sequ_num, last_sequ_ptr);
    -- if is defult instruction group
    if inst_group /= 1 then
      --------------------------------------------------------------------------------
      --  USER Istruction area.  Add all user instructions above this
      --------------------------------------------------------------------------------
      --   if (instruction(1 to len) = "RESET_SYS") then
      if (instruction(1 to len) = "RESET_SYS") then
        wait until tb_clk'event and tb_clk = '1';
        EX_RESET_N  <=  '0';
        wait until tb_clk'event and tb_clk = '1';
        wait until tb_clk'event and tb_clk = '1';
        EX_RESET_N  <=  '1';
        wait for 0 ns;

      --------------------------------------------------------------------------------
      --  SHOW_BIN
      --    Output par1 as a binary number
      elsif (instruction(1 to len) = "SHOW_BIN") then
        txt_print_wvar(defined_vars, txt, bin);
      elsif (instruction(1 to len) = "SHOW_DEC") then
        txt_print_wvar(defined_vars, txt, dec);
      elsif (instruction(1 to len) = "SHOW_HEX") then
        txt_print_wvar(defined_vars, txt, hex);
      --------------------------------------------------------------------------------
      --  READ_PINS
      --    read one of the two output interfaces on the dut, put value in read_data
      --  par1  0  read port1
      --        1  read port2
      elsif (instruction(1 to len) = "READ_PINS") then
        if(par1 = 0) then
          v_read_data :=  EX_DATA1;
        elsif(par1 = 1) then
          v_read_data :=  EX_DATA2;
        else
          assert (false)
            report LF & "Out of range message" & LF
          severity error;
        end if;
      
      --------------------------------------------------------------------------------
      --  READ_VAR
      --    read a variable, put value in read_data
      --  par1 varable name
      elsif (instruction(1 to len) = "READ_VAR") then
        index_variable(defined_vars, par1, temp_int, valid);
        if(valid /= 0) then
          --v_read_data  :=  std_logic_vector(temp_int);
          v_read_data  :=  std_logic_vector(to_signed(temp_int,32));
        else
          assert (false)
            report "READ_VAR Error: Not a valid Variable??"
          severity failure;
        end if;
    
      --------------------------------------------------------------------------------
      --  VERIFY
      --    par1  Data to compare
      elsif (instruction(1 to len) = "VERIFY") then
        v_temp_vec1  :=   std_logic_vector(to_signed(par1,32));
        assert (v_temp_vec1 = v_read_data)
            report "VERIFY instruction found missmatch!" & LF &
                   "Expected data = " & to_hstring(v_temp_vec1) & LF &
                   "Read   data     = " & to_hstring(v_read_data) & LF &
                   "Found on line " & (integer'image(file_line)) & " in file " & file_name
            severity failure;

      --------------------------------------------------------------------------------
      --  PCI_WRITE
      --    par1  address
      --    par2  data
      --  This instruction I use for accessing models.  which is the interface
      --   on the example dut.  I use that interface as a stimulus access port.
      elsif (instruction(1 to len) = "WRITE_DUT") then
        stm_add    <=  std_logic_vector(to_signed(par1,32));
        stm_dat    <=  std_logic_vector(to_signed(par2,32));
        stm_req_n  <=  '0';
        stm_rwn    <=  '0';
        wait until stm_ack_n'event and stm_ack_n = '0';
        stm_req_n  <=  '1';
        wait until stm_ack_n'event and stm_ack_n = '1';
        stm_add   <=  (others => 'Z');
        stm_dat   <=  (others => 'Z');
        stm_rwn   <=  '1';
        wait for 0 ns;

      --------------------------------------------------------------------------------
      --  USER Istruction area.  Add all user instructions above this
      --------------------------------------------------------------------------------
      --------------------------------------------------------------------------
      -- catch those little mistakes
      else
        assert (false)
          report LF & "ERROR:  Seems the command " & instruction(1 to len) & " was defined" & LF &
                      "but not found in the elsif chain, please check the spelling."
          severity failure;
      end if;  -- else if structure end
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--  USER Istruction area.  Add all user instructions below this
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
    else
      --------------------------------------------------------------------------
      --    elsif(instruction(1 to len) = "ABORT") then
      if(inst_idx = 18) then
        assert (false)
          report "The test has aborted due to an error!!"
        severity failure;

      --------------------------------------------------------------------------
      --    elsif(instruction(1 to len) = "FINISH") then
      elsif(inst_idx = 19) then
        report "Test Finished with NO errors!!";
        finish(0);

      --------------------------------------------------------------------------
      --    elsif(instruction(1 to len) = "EQU_VAR") then
      elsif(inst_idx = 2) then
        update_variable(defined_vars, par1, par2, valid);
      
      --------------------------------------------------------------------------
      --    elsif(instruction(1 to len) = "ADD_VAR") then
      elsif(inst_idx = 3) then
        index_variable(defined_vars, par1, temp_int, valid);
        if(valid /= 0) then
          temp_int  :=  temp_int + par2;
          update_variable(defined_vars, par1, temp_int, valid);
        else
          assert (false)
            report "ADD_VAR Error: Not a valid Variable??"
          severity failure;
        end if;
      
      --------------------------------------------------------------------------
      --    elsif(instruction(1 to len) = "SUB_VAR") then
      elsif(inst_idx = 4) then
        index_variable(defined_vars, par1, temp_int, valid);
        if(valid /= 0) then
          temp_int  :=  temp_int - par2;
          update_variable(defined_vars, par1, temp_int, valid);
        else
          assert (false)
            report "SUB_VAR Error: Not a valid Variable??"
          severity failure;
        end if;
      
      --------------------------------------------------------------------------
      --    elsif(instruction(1 to len) = "CALL") then
      elsif(inst_idx = 5) then
        report "CALL in depth:" & integer'image(stack_ptr);
        report "CALL to line:" & integer'image(par1);
        if(stack_ptr >= 7) then
          assert (false)
            report "Call Error: Stack over run, calls to deeply nested!!"
          severity failure;
        end if;
        stack(stack_ptr)  :=  v_line;
        stack_ptr  :=  stack_ptr + 1;
        v_line       :=  par1 - 1;
        report "Call to line: " & integer'image(v_line);
      
      --------------------------------------------------------------------------
      --    elsif(instruction(1 to len) = "RETURN_CALL") then
      elsif(inst_idx = 6) then
        report "CALL out depth:" & integer'image(stack_ptr);
        if(stack_ptr <= 0) then
          assert (false)
            report "Call Error: Stack under run??"
          severity failure;
        end if;
        stack_ptr  :=  stack_ptr - 1;
        v_line  :=  stack(stack_ptr);
        report "Return to line: " & integer'image(v_line);

      --------------------------------------------------------------------------
      --    elsif(instruction(1 to len) = "JUMP") then
      elsif(inst_idx = 7) then
        v_line    :=  par1 - 1;
        wh_state  :=  false;
        wh_stack  := (others => 0);
        wh_dpth   := 0;
        wh_ptr    := 0;
        stack     := (others => 0);
        stack_ptr := 0;

      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "LOOP") then
      elsif (inst_idx = 8) then
     loop_num := loop_num + 1;
        loop_line(loop_num) := v_line;
        curr_loop_count(loop_num) := 0;
        term_loop_count(loop_num) := par1;
      
      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "END_LOOP") then
      elsif (inst_idx = 9) then
        curr_loop_count(loop_num) := curr_loop_count(loop_num) + 1;
        if (curr_loop_count(loop_num) = term_loop_count(loop_num)) then
          loop_num := loop_num - 1;
        else
          v_line := loop_line(loop_num);
        end if;

      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "IF") then
      elsif (inst_idx = 10) then
        if_state  :=  false;
        case par2 is
          when 0 => if(par1 = par3) then if_state  :=  true; end if;
          when 1 => if(par1 > par3) then if_state  :=  true; end if;
          when 2 => if(par1 < par3) then if_state  :=  true; end if;
          when 3 => if(par1 /= par3) then if_state :=  true; end if;
          when 4 => if(par1 >= par3) then if_state :=  true; end if;
          when 5 => if(par1 <= par3) then if_state :=  true; end if;
          when others =>
            assert (false)
              report LF & "ERROR:  IF instruction got an unexpected value" &
                  LF & "  in parameter 2!" & LF &
                   "Found on line " & (tb_to_str(file_line,dec)) & " in file " & file_name
              severity failure;
        end case;

        if(if_state = false) then
          v_line := v_line + 1;
          access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
              inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
              file_name, file_line, last_sequ_num, last_sequ_ptr);
          while(instruction(1 to len) /= "ELSE" and
                instruction(1 to len) /= "ELSEIF" and
                instruction(1 to len) /= "END_IF") loop
            if(v_line < inst_sequ.num_of_lines) then
              v_line := v_line + 1;
              access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
                  inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
                  file_name, file_line, last_sequ_num, last_sequ_ptr);
            else
              assert (false)
               report LF & "ERROR:  IF instruction unable to find terminating" &
                      LF & "    ELSE, ELSEIF or END_IF statement."
               severity failure;             
            end if;
          end loop;
          v_line := v_line - 1;  -- re-align so it will be operated on.
        end if;
       
      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "ELSEIF") then
      elsif (inst_idx = 12) then
        if(if_state = true) then  -- if the if_state is true then skip to the end
          v_line := v_line + 1;
          access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
              inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
              file_name, file_line, last_sequ_num, last_sequ_ptr);
          while(instruction(1 to len) /= "END_IF") loop
            if(v_line < inst_sequ.num_of_lines) then
              v_line := v_line + 1;
              access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
                 inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
                 file_name, file_line, last_sequ_num, last_sequ_ptr);
            else
              assert (false)
               report LF & "ERROR:  IF instruction unable to find terminating" &
                      LF & "    ELSE, ELSEIF or END_IF statement."
               severity failure;             
            end if;
          end loop;
          v_line := v_line - 1;  -- re-align so it will be operated on.
       
        else
          case par2 is
            when 0 => if(par1 = par3) then if_state  :=  true; end if;
            when 1 => if(par1 > par3) then if_state  :=  true; end if;
            when 2 => if(par1 < par3) then if_state  :=  true; end if;
            when 3 => if(par1 /= par3) then if_state :=  true; end if;
            when 4 => if(par1 >= par3) then if_state :=  true; end if;
            when 5 => if(par1 <= par3) then if_state :=  true; end if;
            when others =>
              assert (false)
                report LF & "ERROR:  ELSEIF instruction got an unexpected value" &
                    LF & "  in parameter 2!" & LF &
                     "Found on line " & (tb_to_str(file_line,dec)) & " in file " & file_name
                severity failure;
          end case;
       
          if(if_state = false) then
            v_line := v_line + 1;
            access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
                inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
                file_name, file_line, last_sequ_num, last_sequ_ptr);
            while(instruction(1 to len) /= "ELSE" and
                  instruction(1 to len) /= "ELSEIF" and
                  instruction(1 to len) /= "END_IF") loop
              if(v_line < inst_sequ.num_of_lines) then
                v_line := v_line + 1;
                access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
                    inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
                    file_name, file_line, last_sequ_num, last_sequ_ptr);
              else
                assert (false)
                 report LF & "ERROR:  ELSEIF instruction unable to find terminating" &
                        LF & "    ELSE, ELSEIF or END_IF statement."
                 severity failure;             
              end if;
            end loop;
            v_line := v_line - 1;  -- re-align so it will be operated on.
          end if;
        end if;
       
      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "ELSE") then
      elsif (inst_idx = 11) then
        if(if_state = true) then  -- if the if_state is true then skip the else
          v_line := v_line + 1;
          access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
              inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
              file_name, file_line, last_sequ_num, last_sequ_ptr);
          while(instruction(1 to len) /= "END_IF") loop
            if(v_line < inst_sequ.num_of_lines) then
              v_line := v_line + 1;
              access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
                  inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
                  file_name, file_line, last_sequ_num, last_sequ_ptr);
            else
              assert (false)
               report LF & "ERROR:  IF instruction unable to find terminating" &
                      LF & "    ELSE, ELSEIF or END_IF statement."
               severity failure;             
            end if;
          end loop;
          v_line := v_line - 1;  -- re-align so it will be operated on.
        end if;
       
      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "END_IF") then
      elsif (inst_idx = 13) then
        null;  -- instruction is a place holder for finding the end of IF.
     
      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "WHILE") then
      elsif (inst_idx = 14) then
        wh_state  :=  false;
        case par2 is
          when 0 => if(par1 = par3)  then wh_state  :=  true; end if;
          when 1 => if(par1 > par3)  then wh_state  :=  true; end if;
          when 2 => if(par1 < par3)  then wh_state  :=  true; end if;
          when 3 => if(par1 /= par3) then wh_state :=  true; end if;
          when 4 => if(par1 >= par3) then wh_state :=  true; end if;
          when 5 => if(par1 <= par3) then wh_state :=  true; end if;
          when others =>
            assert (false)
              report LF & "ERROR:  WHILE instruction got an unexpected value" &
                  LF & "  in parameter 2!" & LF &
                   "Found on line " & (tb_to_str(file_line,dec)) & " in file " & file_name
              severity failure;
        end case;

        if(wh_state = true) then
          wh_stack(wh_ptr) :=  v_line;
          wh_ptr  := wh_ptr + 1;
        else
          wh_end := false;
          while(wh_end /= true) loop
            if(v_line < inst_sequ.num_of_lines) then
              v_line := v_line + 1;
              access_inst_sequ(inst_sequ, defined_vars, file_list, v_line, inst_group,
                  inst_idx, instruction, par1, par2, par3, par4, par5, par6, txt, len,
                  file_name, file_line, last_sequ_num, last_sequ_ptr);
            else
              assert (false)
                report LF & "ERROR:  WHILE instruction unable to find terminating" &
                       LF & "    END_WHILE statement."
              severity failure;             
            end if;
           
            -- if is a while need to escape it
            if(instruction(1 to len) = "WHILE") then
              wh_dpth := wh_dpth + 1;
            -- if is the end_while we are looking for
            elsif(instruction(1 to len) = "END_WHILE") then
              if(wh_dpth = 0) then
                wh_end := true;
              else
                wh_dpth := wh_dpth - 1;
              end if;
            end if;
          end loop;
        end if;

      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "END_WHILE") then
      elsif (inst_idx = 15) then
        if(wh_ptr > 0) then
          v_line  :=  wh_stack(wh_ptr - 1) - 1;
          wh_ptr  := wh_ptr - 1;
        end if;
       
      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "MESSAGES_OFF") then
      elsif (inst_idx = 16) then
        messages  := FALSE;
      --------------------------------------------------------------------------------
      --     elsif (instruction(1 to len) = "MESSAGES_ON") then
      elsif (inst_idx = 17) then
        messages  := TRUE;
      else
        report "Unknown defualt instruciton??" severity failure;
      end if;
    end if;  --  end groups
    if (txt(1) /= nul and messages) then
      txt_print_wvar(defined_vars, txt, hex);
    end if;
  end loop;  -- Main Loop end
  
  assert (false)
    report LF & "The end of the simulation! It was not terminated as expected." & LF
  severity failure;
        
  end process Read_file;


end bhv;

