-------------------------------------------------------------------------------
--  File name : testbench_s27kl0641_verilog.vhd
-------------------------------------------------------------------------------
--  Copyright (C) 2015 Spansion, LLC.
--
--  MODIFICATION HISTORY :
--
--  version:  |   author:       | mod date: |   changes made:
--    V1.0      M.Stojanovic     15 June 17     Initial release
--
-------------------------------------------------------------------------------
--  PART DESCRIPTION:
--
--  Description:
--            Generic test enviroment for verification of Spansion RPC PSRAM
--            VITAL model s27kl0641
--
-------------------------------------------------------------------------------
--  Comments :
--      For correct simulation, simulator resolution should be set to 1ps
--
-------------------------------------------------------------------------------
--  Known Bugs:
--
-------------------------------------------------------------------------------
LIBRARY IEEE;
    USE IEEE.std_logic_1164.ALL;
    USE IEEE.VITAL_timing.ALL;
    USE IEEE.VITAL_primitives.ALL;
    USE STD.textio.ALL;

LIBRARY FMF;
    USE FMF.gen_utils.all;
    USE FMF.conversions.all;

LIBRARY work;
    USE work.spansion_tc_pkg.all;

-------------------------------------------------------------------------------
-- ENTITY DECLARATION
-------------------------------------------------------------------------------
ENTITY testbench_s27kl0641_verilog IS

END testbench_s27kl0641_verilog;

-------------------------------------------------------------------------------
-- ARCHITECTURE DECLARATION
-------------------------------------------------------------------------------
ARCHITECTURE vhdl_behavioral of testbench_s27kl0641_verilog IS
COMPONENT s27kl0641
    GENERIC (
       -- memory file to be loaded
        mem_file_name       : STRING    := "s27kl0641.mem";
        UserPreload         : INTEGER   := 0;

        -- For FMF SDF technology file usage
        TimingModel          : STRING
    );

    PORT (
        DQ7             : INOUT std_logic := 'U'; --
        DQ6             : INOUT std_logic := 'U'; --
        DQ5             : INOUT std_logic := 'U'; --
        DQ4             : INOUT std_logic := 'U'; --
        DQ3             : INOUT std_logic := 'U'; --
        DQ2             : INOUT std_logic := 'U'; --
        DQ1             : INOUT std_logic := 'U'; --
        DQ0             : INOUT std_logic := 'U'; --

        CSNeg           : IN    std_ulogic := 'U';
        CK              : IN    std_ulogic := 'U';
        RESETNeg        : IN    std_ulogic := 'U';
        RWDS            : INOUT std_logic  := 'U'
    );
    END COMPONENT;

    FOR ALL: s27kl0641 USE ENTITY WORK.s27kl0641;

    ---------------------------------------------------------------------------
    --memory configuration
    ---------------------------------------------------------------------------
    CONSTANT MaxData        :NATURAL := 16#FF#;
    CONSTANT MemSize        :NATURAL := 16#3FFFFF#;
    CONSTANT HiAddrBit      :NATURAL := 21;
    ---------------------------------------------------------------------------
    --model configuration
    ---------------------------------------------------------------------------
    CONSTANT mem_file        : STRING  := "s27kl0641.mem";
    CONSTANT half_period_100 : time    := 5.1 ns; -- 1/(2*100MHz)
    CONSTANT UserPreload     : INTEGER := 1;
    CONSTANT TimingModel     : STRING  := "S27KL0641DABHI000";
    ---------------------------------------------------------------------------

     --Flash Memory Array
    TYPE MemArr IS ARRAY (0 TO (2*MemSize + 1)) OF INTEGER RANGE -1 TO MaxData;
    SHARED VARIABLE Mem     : MemArr   := (OTHERS => MaxData);

    SIGNAL Config_reg : std_logic_vector(15 downto 0) := "1000111100011111";

    SIGNAL check_err       :   std_logic := '0'; -- Active high on error
    SIGNAL ErrorInTest     :   std_logic := '0';

    --DUT port
    SIGNAL T_DQ         : std_logic_vector(7 downto 0) := (OTHERS => 'U');
    SIGNAL T_CSNeg      : std_ulogic := 'U';
    SIGNAL T_CK         : std_ulogic := 'U';
    SIGNAL T_RESETNeg   : std_ulogic := '1';
    SIGNAL T_RWDS       : std_logic  := 'U';

    SHARED VARIABLE CA_bits      : std_logic_vector(47 downto 0);
    SHARED VARIABLE BurstDelay   : NATURAL RANGE 0 TO 6;
    SHARED VARIABLE RefreshDelay : NATURAL RANGE 0 TO 6;
    SHARED VARIABLE BurstLength  : NATURAL RANGE 0 TO 32;
    SIGNAL half_period           : TIME  := half_period_100;
    SIGNAL half_period_tmp       : TIME;
    SIGNAL change_ck             : std_logic := '0';

    SHARED VARIABLE WrAddress       : NATURAL RANGE 0 to MemSize+1  :=0;
    SHARED VARIABLE WrAddress_Start : NATURAL RANGE 0 to MemSize  :=0;
    SHARED VARIABLE WrAddress_slv   : std_logic_vector(HiAddrBit downto 0);

    SHARED VARIABLE WR_ADDR_WRAP    : BOOLEAN := FALSE;
    SHARED VARIABLE WR_LEGACY_WRAP  : BOOLEAN := FALSE;
    SHARED VARIABLE WR_CONTINUOUS   : BOOLEAN := FALSE;

    SHARED VARIABLE ClockEnable      : BOOLEAN := FALSE;

    SHARED VARIABLE cmd_seq   : cmd_seq_type;

    SIGNAL data_num    : NATURAL RANGE 0 TO MemSize;
    SIGNAL cmd             : cmd_type := idle;

    SIGNAL Tseries     : NATURAL;
    SIGNAL Tcase       : NATURAL;

    SHARED VARIABLE ts_cnt  :   NATURAL RANGE 1 TO 40:=1; -- testseries counter
    SHARED VARIABLE tc_cnt  :   NATURAL RANGE 0 TO 15:=0; -- testcase counter

    BEGIN
        DUT : s27kl0641
    GENERIC MAP (
            -- memory file to be loaded
            UserPreload         => UserPreload,
            mem_file_name       => "s27kl0641.mem",

            -- For FMF SDF technology file usage
            TimingModel         =>  "S27KL0641DABHI000"
        )

        PORT MAP(
            DQ7        => T_DQ(7), --
            DQ6        => T_DQ(6), --
            DQ5        => T_DQ(5), --
            DQ4        => T_DQ(4), --
            DQ3        => T_DQ(3), --
            DQ2        => T_DQ(2), --
            DQ1        => T_DQ(1), --
            DQ0        => T_DQ(0), --

            CSNeg      => T_CSNeg,
            CK         => T_CK,
            RESETNeg   => T_RESETNeg,
            RWDS       => T_RWDS
        );

    half_period <= half_period_tmp WHEN  change_ck = '1' ELSE
                   half_period_100 WHEN
                            ((TimingModel = "S27KL0641DABHI000") OR
                             (TimingModel = "s27kl0641dabhi000"));

    clk_generation: PROCESS(T_CK, T_CSNeg)
    BEGIN
        IF T_CSNeg = '1' THEN
            T_CK    <= '0';
        ELSIF (ClockEnable) THEN
            T_CK    <= NOT T_CK    AFTER half_period;
        END IF;
    END PROCESS clk_generation;

    ----------------------------------------------------------------------------
    --At the end of the simulation, if not ErrorInTest there were no errors
    ----------------------------------------------------------------------------
    err_ctrl : PROCESS ( check_err  )
    BEGIN
        IF check_err = '1' THEN
            ErrorInTest <= '1';
        END IF;
    END PROCESS err_ctrl;

tb  :PROCESS
 --------------------------------------------------------------------------
    --  PROCEDURE to select TC
    -- can be modified to read TC list from file, or to generate random list
    --------------------------------------------------------------------------
    PROCEDURE   Pick_TC
        (Model   :  IN  STRING  := "S27KL0641DABHI000" )
    IS
    BEGIN
        IF TC_cnt < tc(TS_cnt) THEN
            TC_cnt := TC_cnt+1;
        ELSE
            TC_cnt := 1;
            IF TS_cnt < 4 THEN
                TS_cnt := TS_cnt+1;
            ELSE
                IF ErrorInTest='0' THEN
                    REPORT "Test Ended without errors"
                    SEVERITY note;
                ELSE
                    REPORT "There were errors in test"
                    SEVERITY note;
                END IF;
                WAIT;
            END IF;
        END IF;
    END PROCEDURE Pick_TC;

   ----------------------------------------------------------------------------
    --bus commands, device specific implementation
    ---------------------------------------------------------------------------
    TYPE bus_type IS (bus_idle,
                      bus_standby,  --CS# deasserted,others are don't care
                      bus_enable,   --CS# asserted,others deasserted
                      bus_reset,
                      bus_data_reg,
                      bus_data_mem,
                      bus_latency,
                      bus_ca,
                      bus_read);

    TYPE target_type IS (memdata, reg);
    TYPE brst_type IS (wrap, cont);
    TYPE op_type IS (write, read);

    --bus drive for specific command sequence cycle
    PROCEDURE bus_cycle(
        bus_cmd :IN   bus_type := bus_idle;
        rw      :IN   op_type        :=  write;
        target  :IN   target_type        :=  memdata;
        burst_type :IN   brst_type        :=  wrap;
        data_lo :IN   INTEGER RANGE -1 TO MaxData  := -1; -- -1 for all Z
        data_hi :IN   INTEGER RANGE -1 TO MaxData  := -1; -- -1 for all Z
        address :IN   NATURAL RANGE  0 TO MemSize  :=  0;
        ubm     :IN   NATURAL RANGE  0 TO 1  :=  0; -- Upper Byte Mask
        lbm     :IN   NATURAL RANGE  0 TO 1  :=  0; -- Lower Byte Mask
        tm      :IN   TIME                         := 0 ns)
    IS
        VARIABLE tmpA     : std_logic_vector(21 downto 0);
    BEGIN

        IF data_lo =-1 OR data_hi =-1 THEN -- HiZ
            T_DQ(7 downto 0) <= (OTHERS => 'Z');
            T_RWDS <=  'Z';
        END IF;

        tmpA := to_slv(address, 22);
        CA_bits(34 downto 16) := tmpA(21 downto 3);
        CA_bits(2 downto 0)   := tmpA(2 downto 0);

        IF rw = write THEN
            CA_bits(47) := '0';
        ELSIF rw = read THEN
            CA_bits(47) := '1';
        END IF;

        IF target = memdata THEN
            CA_bits(46) := '0';
        ELSIF target = reg THEN
            CA_bits(46) := '1';
        END IF;

        IF burst_type = wrap THEN
            CA_bits(45) := '0';
        ELSIF burst_type = cont THEN
            CA_bits(45) := '1';
        END IF;

        IF (Config_reg(1 DOWNTO 0) = "00") OR
        (Config_reg(1 DOWNTO 0) = "01") THEN
            BurstLength := 32;
        ELSIF Config_reg(1 DOWNTO 0) = "10" THEN
            BurstLength := 8;
        ELSIF Config_reg(1 DOWNTO 0) = "11" THEN
            BurstLength := 16;
        END IF;

        IF (Config_reg(7 downto 4)) = "0000"  THEN
            BurstDelay := 5;
            RefreshDelay := 5;
        ELSIF (Config_reg(7 downto 4)) = "0001"  THEN
            BurstDelay := 6;
            RefreshDelay := 6;
        ELSIF (Config_reg(7 downto 4)) = "1111"  THEN
            BurstDelay := 4;
            RefreshDelay := 4;
        END IF;

        CASE bus_cmd IS

            WHEN bus_idle =>
                ClockEnable:= TRUE;
                T_RESETNeg <= '1';
                T_CSNeg    <= '1';
                WAIT FOR 30 ns;

            WHEN bus_standby =>
                T_CSNeg    <= '1';
                WAIT FOR 30 ns;

            WHEN bus_enable =>
                T_CSNeg    <= '0';
                CA_bits := (OTHERS => '0');

                WAIT FOR tm ;

            WHEN bus_reset =>
                T_RESETNeg <= '0', '1' AFTER tm ;

            WHEN bus_latency =>

                IF Config_reg(3)='1' THEN -- fixed latency
                    FOR I IN RefreshDelay-2 DOWNTO 0 LOOP
                        WAIT UNTIL falling_edge(T_CK);
                    END LOOP;
                    FOR I IN BurstDelay-1 DOWNTO 0 LOOP
                        WAIT UNTIL falling_edge(T_CK);
                    END LOOP;
                ELSIF Config_reg(3)='0' THEN -- variable latency
                    FOR I IN BurstDelay-2 DOWNTO 0 LOOP
                        WAIT UNTIL falling_edge(T_CK);
                    END LOOP;
                END IF;
                WAIT FOR half_period/2;

            WHEN bus_data_reg =>
                T_DQ <= to_slv(data_hi,8);
                WAIT UNTIL rising_edge(T_CK);
                WAIT FOR half_period/2;
                T_DQ <= to_slv(data_lo,8);
                WAIT UNTIL falling_edge(T_CK);
                WAIT FOR half_period/2;

                T_DQ(7 downto 0) <= (OTHERS => 'Z');

            WHEN bus_data_mem =>
                T_RWDS <= to_sl(ubm);
                T_DQ <= to_slv(data_hi,8);
                WAIT UNTIL rising_edge(T_CK);
                WAIT FOR half_period/2;
                T_RWDS <= to_sl(lbm);
                T_DQ <= to_slv(data_lo,8);
                WAIT UNTIL falling_edge(T_CK);
                WAIT FOR half_period/2;

            WHEN bus_ca =>
                FOR I IN 6 DOWNTO 1 LOOP
                    T_DQ <= CA_bits(8*i-1 DOWNTO 8*i-8);
                    WAIT UNTIL T_CK'EVENT;
                    WAIT FOR half_period/2;
                END LOOP;

            WHEN bus_read =>
                T_DQ(7 downto 0)      <= (OTHERS => 'Z');

                IF Config_reg(3)='1' THEN -- fixed latency
                    FOR I IN RefreshDelay-2 DOWNTO 0 LOOP
                        WAIT UNTIL falling_edge(T_CK);
                    END LOOP;
                    FOR I IN BurstDelay-1 DOWNTO 0 LOOP
                        WAIT UNTIL falling_edge(T_CK);
                    END LOOP;
                ELSIF Config_reg(3)='0' THEN -- variable latency
                    FOR I IN BurstDelay-2 DOWNTO 0 LOOP
                        WAIT UNTIL falling_edge(T_CK);
                    END LOOP;
                END IF;

                FOR I IN data_num-1 downto 0 LOOP
                    WAIT UNTIL falling_edge(T_CK);
                END LOOP;
                WAIT UNTIL falling_edge(T_CK);
                ClockEnable:= FALSE; --Clock Generator is disabled

                WAIT FOR 10 ns;
        END CASE;
    END PROCEDURE;

    ----------------------------------------------------------------------------
    --procedure to decode commands into specific bus command sequence
    ---------------------------------------------------------------------------
    PROCEDURE cmd_dc
        (   command  :   IN  cmd_rec   )
    IS
        VARIABLE    D_lo    : INTEGER RANGE  -1 to MaxData;
        VARIABLE    D_hi    : INTEGER RANGE  -1 to MaxData;
        VARIABLE    Addr    : NATURAL RANGE 0 to MemSize  :=0;
        VARIABLE    lbm, ubm : NATURAL RANGE  0 to 1;
        VARIABLE    i        : NATURAL;
    BEGIN
        CASE command.cmd IS

            WHEN    idle        =>
                bus_cycle(bus_cmd => bus_idle);

            WHEN h_reset =>
                Config_reg(15 downto 0)  <= "1000111100011111";
                bus_cycle(bus_cmd => bus_reset,
                          tm      => command.wtime);

            WHEN wr_mem_cont =>
                Addr :=  command.addr;
                D_lo :=  command.data_lo;
                D_hi :=  command.data_hi;
                lbm  :=  command.lbm;
                ubm  :=  command.ubm;

                bus_cycle(bus_cmd => bus_enable);

                bus_cycle(bus_cmd => bus_ca,
                          rw      => write,
                          target  => memdata,
                          burst_type => cont,
                          address => Addr,
                          tm      => command.wtime);

                WR_LEGACY_WRAP := FALSE;
                WR_ADDR_WRAP   := FALSE;
                WR_CONTINUOUS  := FALSE;

                WrAddress := Addr;
                WrAddress_Start := Addr;

                bus_cycle(bus_cmd => bus_latency);

            WHEN wr_mem_wrap =>
                Addr :=  command.addr;
                D_lo :=  command.data_lo;
                D_hi :=  command.data_hi;
                lbm  :=  command.lbm;
                ubm  :=  command.ubm;

                bus_cycle(bus_cmd => bus_enable);

                bus_cycle(bus_cmd => bus_ca,
                          rw      => write,
                          target  => memdata,
                          burst_type => wrap,
                          address => Addr,
                          tm      => command.wtime);

                WR_LEGACY_WRAP := FALSE;
                WR_ADDR_WRAP   := FALSE;
                WR_CONTINUOUS  := FALSE;

                WrAddress := Addr;
                WrAddress_Start := Addr;

                bus_cycle(bus_cmd => bus_latency);

            WHEN w_data_cont | w_data_wrap =>
                D_lo :=  command.data_lo;
                D_hi :=  command.data_hi;
                lbm  :=  command.lbm;
                ubm  :=  command.ubm;

                IF command.cmd =  w_data_cont THEN
                    bus_cycle(bus_cmd => bus_data_mem,
                            data_lo  => D_lo,
                            data_hi  => D_hi,
                            lbm      => lbm,
                            ubm      => ubm,
                            burst_type => cont);
                ELSIF command.cmd =  w_data_wrap THEN
                    bus_cycle(bus_cmd => bus_data_mem,
                            data_lo  => D_lo,
                            data_hi  => D_hi,
                            lbm      => lbm,
                            ubm      => ubm,
                            burst_type => wrap);
                END IF;

                IF lbm = 0 AND command.aux = valid THEN
                    Mem(2*WrAddress) := command.data_lo;
                END IF;
                IF ubm = 0 AND command.aux = valid THEN
                    Mem(2*WrAddress+1) := D_hi;
                END IF;

                IF CA_bits(45) = '1' OR WR_CONTINUOUS THEN -- continuous burst
                    IF WrAddress = MemSize THEN
                        WrAddress := 0;
                    ELSE
                        WrAddress := WrAddress + 1;
                    END IF;

                ELSIF CA_bits(45) = '0' THEN -- wrapped burst
                    IF Config_reg(2) = '1' THEN -- legacy wrapped manner
                        IF BurstLength=8 OR BurstLength=16 OR
                        (Config_reg(1 downto 0) = "00") OR WR_LEGACY_WRAP THEN
                            WrAddress := WrAddress + 1;
                            IF WrAddress mod BurstLength = 0 THEN
                                WrAddress := WrAddress - BurstLength;
                            END IF;
                        ELSIF Config_reg(1 DOWNTO 0) = "01" THEN -- alternate
                            WrAddress := WrAddress + 1;
                            WrAddress_slv := to_slv(WrAddress, HiAddrBit+1);
                            IF WrAddress mod (BurstLength/2)= 0 THEN
                                IF NOT WR_ADDR_WRAP THEN
                                    WrAddress := WrAddress-(BurstLength/2);
                                    WR_ADDR_WRAP := TRUE;
                                ELSE
                                    WR_LEGACY_WRAP := TRUE;
                                    IF WrAddress_slv(4) = '0' THEN
                                        WrAddress := WrAddress-BurstLength;
                                    END IF;
                                END IF;
                            END IF;
                            IF WrAddress =WrAddress_Start AND NOT WR_LEGACY_WRAP THEN
                                WrAddress_slv := to_slv(WrAddress, HiAddrBit+1);
                                IF WrAddress_slv(4) = '0' THEN
                                    WrAddress := (WrAddress_Start/BurstLength)*BurstLength
                                                                + BurstLength/2;
                                ELSE
                                    WrAddress := (WrAddress_Start/BurstLength)*BurstLength;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Config_reg(2) = '0' THEN -- hybrid burst
                        IF BurstLength=8 OR BurstLength=16 OR
                        (Config_reg(1 downto 0) = "00") THEN
                            WrAddress := WrAddress + 1;
                            WrAddress_slv := to_slv(WrAddress, HiAddrBit+1);
                            IF WrAddress mod BurstLength = 0 THEN
                                WrAddress := WrAddress - BurstLength;
                            END IF;
                            IF WrAddress =WrAddress_Start THEN
                                WrAddress :=(WrAddress_Start/BurstLength)*BurstLength
                                                                + BurstLength;
                                IF WrAddress = MemSize + 1 THEN
                                    WrAddress := 0;
                                END IF;
                                WR_CONTINUOUS := TRUE;
                            END IF;
                        ELSIF Config_reg(1 downto 0) = "01" THEN -- alternate
                            WrAddress := WrAddress + 1;
                            IF WrAddress mod (BurstLength/2)= 0 THEN
                                IF NOT WR_ADDR_WRAP THEN
                                    WrAddress := WrAddress-(BurstLength/2);
                                    WR_ADDR_WRAP := TRUE;
                                ELSE
                                    WrAddress :=(WrAddress_Start/BurstLength)*BurstLength
                                                                + BurstLength;
                                    IF WrAddress = MemSize + 1 THEN
                                        WrAddress := 0;
                                    END IF;
                                    WR_CONTINUOUS := TRUE;
                                END IF;
                            END IF;
                            IF WrAddress =WrAddress_Start THEN
                                WrAddress_slv := to_slv(WrAddress, HiAddrBit+1);
                                IF WrAddress_slv(4) = '1' THEN
                                    WrAddress:=(WrAddress_Start/BurstLength)*BurstLength;
                                ELSE
                                    WrAddress:=(WrAddress_Start/BurstLength)*BurstLength
                                                                + BurstLength/2;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                END IF;

            WHEN wr_reg =>
                Addr :=  command.addr;
                D_lo :=  command.data_lo;
                D_hi :=  command.data_hi;

                bus_cycle(bus_cmd => bus_enable);

                bus_cycle(bus_cmd => bus_ca,
                          rw      => write,
                          target  => reg,
                          tm      => command.wtime);

                bus_cycle(bus_cmd => bus_data_reg,
                         data_lo  => D_lo,
                         data_hi  => D_hi);

                bus_cycle(bus_cmd => bus_standby);

                Config_reg(7 downto 0) <= to_slv(D_lo, 8);
                Config_reg(15 downto 8) <= to_slv(D_hi, 8);

            WHEN  rd_mem_wrap | rd_mem_cont | rd_reg |
                    rd_HiZ =>
                Addr :=  command.addr;

                bus_cycle(bus_cmd => bus_enable);

                IF command.cmd = rd_mem_wrap THEN
                    bus_cycle(bus_cmd => bus_ca,
                            rw => read,
                            target  => memdata,
                            burst_type => wrap,
                            address => Addr);
                ELSIF command.cmd = rd_mem_cont OR
                      command.cmd = rd_HiZ THEN
                    bus_cycle(bus_cmd => bus_ca,
                            rw => read,
                            target  => memdata,
                            burst_type => cont,
                            address => Addr);
                ELSIF command.cmd = rd_reg THEN
                    bus_cycle(bus_cmd => bus_ca,
                            rw => read,
                            target  => reg,
                            burst_type => cont);
                END IF;

                bus_cycle(bus_cmd => bus_read);

                bus_cycle(bus_cmd => bus_standby);

            WHEN clock =>
                IF command.wtime /= 0 ns THEN
                    change_ck <= '1';
                    half_period_tmp <= command.wtime/2;
                ELSE
                    change_ck <= '0';
                END IF;

            WHEN    wt          =>
                WAIT FOR command.wtime;

            WHEN    OTHERS    => NULL;
            WAIT FOR command.wtime;
        END CASE;
    END PROCEDURE cmd_dc;

    VARIABLE cmd_cnt    :   NATURAL;
    VARIABLE command    :   cmd_rec;

BEGIN
    TestInit(TimingModel);
    Pick_TC (Model   =>  TimingModel);

    Tseries <=  ts_cnt  ;
    Tcase   <=  tc_cnt  ;

    Generate_TC
        (Model       => TimingModel,
         Series      => ts_cnt,
         TestCase    => tc_cnt,
         command_seq => cmd_seq);

    cmd_cnt := 1;

    WHILE cmd_seq(cmd_cnt).cmd /= done LOOP
        command  := cmd_seq(cmd_cnt);
        cmd      <=  command.cmd;
        data_num <= command.data_num;
        cmd_dc(command);
        cmd_cnt :=cmd_cnt + 1;
    END LOOP;

END PROCESS tb;

-------------------------------------------------------------------------------
-- Checker process,
-------------------------------------------------------------------------------
checker: PROCESS

    VARIABLE Data_reg    : std_logic_vector(15 downto 0);
    VARIABLE RdAddress       :   NATURAL;
    VARIABLE RdAddress_Start :   NATURAL;

    VARIABLE RD_ADDR_WRAP    : BOOLEAN := FALSE;
    VARIABLE RD_LEGACY_WRAP  : BOOLEAN := FALSE;
    VARIABLE RD_CONTINUOUS   : BOOLEAN := FALSE;
    VARIABLE RdAddress_slv   : std_logic_vector(HiAddrBit downto 0);

BEGIN

    IF (T_CSNeg='0') THEN
        RD_ADDR_WRAP    := FALSE;
        RD_LEGACY_WRAP  := FALSE;
        RD_CONTINUOUS   := FALSE;

        IF (cmd = rd_mem_cont) OR (cmd = rd_mem_wrap) OR
        (cmd = rd_reg) THEN
            FOR I IN 3 DOWNTO 0 LOOP
                WAIT UNTIL T_CK'EVENT;
            END LOOP;

            RdAddress := to_nat(CA_bits(34 downto 16) & CA_bits(2 downto 0));
            RdAddress_Start := RdAddress;
            RdAddress_slv := to_slv(RdAddress, 22);
                --Latency Clocks
            IF Config_reg(3)='1' THEN -- fixed latency
                FOR I IN RefreshDelay-1 DOWNTO 0 LOOP
                    WAIT UNTIL falling_edge(T_CK);
                END LOOP;
                FOR I IN BurstDelay-1 DOWNTO 0 LOOP
                    WAIT UNTIL falling_edge(T_CK);
                END LOOP;
            ELSIF Config_reg(3)='0' THEN -- variable latency
                FOR I IN BurstDelay-1 DOWNTO 0 LOOP
                    WAIT UNTIL falling_edge(T_CK);
                END LOOP;
            END IF;

            FOR I IN data_num-1 DOWNTO 0 LOOP
                Data_reg(15 downto 0) := (OTHERS => '0');
                IF I=data_num-1 THEN
                    WAIT UNTIL rising_edge(t_CK);
                    WAIT FOR 6.15 ns;
                    IF half_period = half_period_100 THEN
                        WAIT FOR 1.7 ns;
                    END IF;
                ELSE
                    WAIT FOR half_period;
                END IF;
                Data_reg(15 DOWNTO 8) := T_DQ;

                IF cmd /= rd_HiZ THEN
                    Check_RDS(
                        RDS       => T_RWDS,
                        RDSValue  => '1',
                        check_err => check_err);
                END IF;

                WAIT FOR half_period;
                Data_reg(7 DOWNTO 0) := T_DQ;

                IF cmd /= rd_HiZ THEN
                    Check_RDS(
                        RDS       => T_RWDS,
                        RDSValue  => '0',
                        check_err => check_err);
                END IF;

                IF cmd = rd_mem_cont OR RD_CONTINUOUS THEN
                    Check_read (
                        DQ       => Data_reg,
                        Data1     => Mem(2*RdAddress),
                        Data2     => Mem(2*RdAddress+1),
                        check_err=> check_err);

                    -- if the highest address is reached
                    IF RdAddress = MemSize THEN
                        RdAddress := 0;
                    ELSE
                        RdAddress:=RdAddress+1;
                    END IF;

                ELSIF cmd = rd_mem_wrap THEN
                    Check_read (
                        DQ       => Data_reg,
                        Data1     => Mem(2*RdAddress),
                        Data2     => Mem(2*RdAddress+1),
                        check_err=> check_err);

                    IF Config_reg(2) = '1' THEN -- legacy wrapped manner
                        IF BurstLength=8 OR BurstLength=16 OR
                        (Config_reg(1 downto 0) = "00") OR RD_LEGACY_WRAP THEN
                            RdAddress := RdAddress + 1;
                            IF RdAddress mod BurstLength = 0 THEN
                                RdAddress := RdAddress - BurstLength;
                            END IF;
                        ELSIF Config_reg(1 DOWNTO 0) = "01" THEN -- alternate
                            RdAddress := RdAddress + 1;
                            RdAddress_slv := to_slv(RdAddress, HiAddrBit+1);
                            IF RdAddress mod (BurstLength/2)= 0 THEN
                                IF NOT RD_ADDR_WRAP THEN
                                    RdAddress := RdAddress-(BurstLength/2);
                                    RD_ADDR_WRAP := TRUE;
                                ELSE
                                    RD_LEGACY_WRAP := TRUE;
                                    IF RdAddress_slv(4) = '0' THEN
                                        RdAddress := RdAddress-BurstLength;
                                    END IF;
                                END IF;
                            END IF;
                            IF (RdAddress = RdAddress_Start) AND NOT RD_LEGACY_WRAP THEN
                                RdAddress_slv := to_slv(RdAddress, HiAddrBit+1);
                                IF RdAddress_slv(4) = '0' THEN
                                    RdAddress := (RdAddress_Start/BurstLength)*BurstLength
                                                                + BurstLength/2;
                                ELSE
                                    RdAddress := (RdAddress_Start/BurstLength)*BurstLength;
                                END IF;
                            END IF;
                        END IF;

                    ELSIF Config_reg(2) = '0' THEN -- hybrid burst
                        IF BurstLength=8 OR BurstLength=16 OR
                        (Config_reg(1 downto 0) = "00") THEN
                            RdAddress := RdAddress + 1;
                            IF RdAddress mod BurstLength = 0 THEN
                                RdAddress := RdAddress - BurstLength;
                            END IF;
                            IF RdAddress =RdAddress_Start THEN
                                RdAddress :=(RdAddress_Start/BurstLength)*BurstLength
                                                                + BurstLength;
                                IF RdAddress = MemSize + 1 THEN
                                    RdAddress := 0;
                                END IF;
                                RD_CONTINUOUS := TRUE;
                            END IF;
                        ELSIF Config_reg(1 downto 0) = "01" THEN -- alternate
                            RdAddress := RdAddress + 1;
                            RdAddress_slv := to_slv(RdAddress, HiAddrBit+1);
                            IF RdAddress mod (BurstLength/2)= 0 THEN
                                IF NOT RD_ADDR_WRAP THEN
                                    RdAddress := RdAddress-(BurstLength/2);
                                    RD_ADDR_WRAP := TRUE;
                                ELSE
                                    RdAddress :=(RdAddress_Start/BurstLength)*BurstLength
                                                                + BurstLength;
                                    IF RdAddress = MemSize + 1 THEN
                                        RdAddress := 0;
                                    END IF;
                                    RD_CONTINUOUS := TRUE;
                                END IF;
                            END IF;
                            IF RdAddress =RdAddress_Start THEN
                                RdAddress_slv := to_slv(RdAddress, HiAddrBit+1);
                                IF RdAddress_slv(4) = '1' THEN
                                    RdAddress:=(RdAddress_Start/BurstLength)*BurstLength;
                                ELSE
                                    RdAddress:=(RdAddress_Start/BurstLength)*BurstLength
                                                                + BurstLength/2;
                                END IF;
                            END IF;
                        END IF;
                    END IF;

                ELSIF cmd = rd_HiZ THEN
                    Check_Z (
                        DQ       => T_DQ,
                        check_err=> check_err);

                ELSIF cmd = rd_reg THEN
                    Check_read (
                        DQ       => Data_reg,
                        Data1     => to_nat(Config_reg(7 DOWNTO 0)),
                        Data2     => to_nat(Config_reg(15 DOWNTO 8)),
                        check_err=>check_err);
                END IF;

            END LOOP;
        END IF;
    END IF;
    WAIT ON T_CSNeg;

END PROCESS checker;

default:    PROCESS
        -- text file input variables
        FILE mem_f             : text  is  mem_file;
        VARIABLE buf           : line;
        VARIABLE addr_ind      : NATURAL;
        VARIABLE ind           : NATURAL := 0;
        VARIABLE data_slv      : std_logic_vector(15 downto 0);
BEGIN

    --Preload Control
    -----------------------------------------------------------------------
    -- File Read Section
    -----------------------------------------------------------------------
    IF UserPreload = 1 THEN
    ----------------------------------------------------------------------------
    -----s27kl0641 memory preload file format ---------------------------------
    ----------------------------------------------------------------------------
    --   /         - comment
    --   @aaaaaaaa - <aaaaaaaa> stands for address within sector
    --   dddd      - <dddd> is word to be written at Mem(*)(aaaaaaaa++)
    --             (aaaaaaaa is incremented at every load)
    --   only first 1-9 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
    ----------------------------------------------------------------------------
        IF (mem_file /= "none") THEN
            addr_ind := 0;
            Mem := (OTHERS => MaxData);
            WHILE (not ENDFILE (mem_f)) LOOP
                READLINE (mem_f, buf);
                IF buf(1) = '/' THEN --comment
                    NEXT;
                ELSIF buf(1) = '@' THEN --address
                    addr_ind := h(buf(2 to 7));
                ELSE
                    IF addr_ind <= MemSize THEN
                        data_slv := to_slv(h(buf(1 to 4)), 16);
                        Mem(2*addr_ind) := to_nat(data_slv(7 DOWNTO 0));
                        Mem(2*addr_ind+1) := to_nat(data_slv(15 DOWNTO 8));
                        addr_ind := addr_ind + 1;
                    END IF;
                END IF;
            END LOOP;
        END IF;
    END IF;

    WAIT;

END PROCESS default;

END vhdl_behavioral;