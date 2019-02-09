-------------------------------------------------------------------------------
--  File name : spansion_tc_pkg.vhd
-------------------------------------------------------------------------------
--  Copyright (C) 2015 Spansion, LLC.
--
--  MODIFICATION HISTORY :
--
--  version: |    author:      |  mod date: | changes made:
--    V1.0      M.Stojanovic    15 June 17   Initial release
--
-------------------------------------------------------------------------------
--  PART DESCRIPTION:
--
--  Description:
--              Generic test environment for verification of RPC-PSRAM
--              VITAL models.
--              For models:
--                  S27KL0641
--
--------------------------------------------------------------------------------
--  Comments :
--      For correct simulation, simulator resolution should be set to 1 ps
--
--------------------------------------------------------------------------------
--  Known Bugs:
--
-------------------------------------------------------------------------------
LIBRARY IEEE;
                USE IEEE.std_logic_1164.ALL;
                USE STD.textio.ALL;

LIBRARY FMF;
                USE FMF.gen_utils.all;
                USE FMF.conversions.all;
-------------------------------------------------------------------------------
-- ENTITY DECLARATION
-------------------------------------------------------------------------------
PACKAGE spansion_tc_pkg IS
    ---------------------------------------------------------------------------
    --Values specified in this section determine wait periods of programming,
    --erase and internal device operation
    --Min, typ and max SDF parameters should all be supported for all timing
    --models.When FTM or SDF values change,and are not supported
    --these values may need to be updated.
    ---------------------------------------------------------------------------
    -- HW reset to read time
    SHARED VARIABLE tRPH        : TIME;
    -- CSNeg hold time from RESETNeg /
    SHARED VARIABLE tRH         : TIME;
    -- RESETNeg low pulse
    SHARED VARIABLE tRP         : TIME;
    -- Power On Reset
    SHARED VARIABLE tVCS        : TIME;
    -- Deep Power Down to Idle wake up time
    SHARED VARIABLE tDPD        : TIME;

    CONSTANT AddrRange          : NATURAL := 16#3FFFFF#;


    ---------------------------------------------------------------------------
    --TC type
    ---------------------------------------------------------------------------
    TYPE TC_type IS RECORD
                        SERIES   : NATURAL RANGE 1 TO 40;
                        TESTCASE : NATURAL RANGE 1 TO 15;
                    END RECORD;

    ----------------------------------------------------------------------------
    -- commands to the device
    ----------------------------------------------------------------------------
    TYPE CMD_TYPE IS (  done,
                        idle,
                        h_reset,
                        wr_mem_cont,
                        wr_mem_wrap,
                        wr_reg,
                        w_data_cont,
                        w_data_wrap,
                        rd_mem_cont,
                        rd_mem_wrap,
                        rd_reg,
                        rd_HiZ,
                        clock,
                        wt);

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    TYPE Aux_type IS (  valid,    -- valid operation
                        violate   -- used to specify attempt write operation
                                  -- that can not be accepted
                      );

    TYPE cmd_rec IS
        RECORD
            cmd     :   cmd_type;
            data_num:   NATURAL;
            data_hi :   INTEGER RANGE  -1 TO 16#FF#;
            data_lo :   INTEGER RANGE  -1 TO 16#FF#;
            ubm     :   NATURAL;
            lbm     :   NATURAL;
            addr    :   NATURAL ;
            aux     :   Aux_type;
            wtime   :   time;  --valid with wt cmd
        END RECORD;

    --number of testcases per testseries
    TYPE tc_list IS  ARRAY (1 TO 4) OF NATURAL;
    CONSTANT tc : tc_list :=
    --0                 1
    --1-2-3-4-5-6-7-8-9-0-1-2-3-4-5-6-7-8-9-0-1
     (1,3,3,1);--

   --TC command sequence
    TYPE cmd_seq_type IS ARRAY(0 TO 600) OF cmd_rec;

    ---------------------------------------------------------------------------
    --PUBLIC
    --PROCEDURE to set DUT dependant parameters
    ---------------------------------------------------------------------------
    PROCEDURE   TestInit
         (  Model       : IN  STRING);
    ---------------------------------------------------------------------------
    --PUBLIC
    --PROCEDURE to generate command sequence
    ---------------------------------------------------------------------------
    PROCEDURE   Generate_TC
         (  Model       : IN  STRING  ;
            Series      : IN  NATURAL RANGE 1 TO 40;
            TestCase    : IN  NATURAL RANGE 1 TO 15;
            command_seq : OUT cmd_seq_type
         );

    ---------------------------------------------------------------------------
    -- PUBLIC
    -- CHECKER PROCEDURES
    ---------------------------------------------------------------------------

    PROCEDURE   Check_Z (
        DQ   :  IN std_logic_vector(7 downto 0);
        SIGNAL check_err:  OUT std_logic);

    PROCEDURE   Check_read (
        DQ    :  IN std_logic_vector(15 downto 0);
        Data1  :  IN NATURAL;
        Data2  :  IN NATURAL;
        SIGNAL check_err:  OUT std_logic);

    PROCEDURE   Check_RDS (
        RDS        :  IN std_logic;
        RDSValue   :  IN std_logic;
        SIGNAL check_err :  OUT std_logic);

 END PACKAGE spansion_tc_pkg;

PACKAGE BODY spansion_tc_pkg IS

     ---------------------------------------------------------------------------
    --Public PROCEDURE to generate command sequence
    ---------------------------------------------------------------------------
    PROCEDURE   TestInit (  Model : IN  STRING) IS
    BEGIN
        tVCS       := 150 us;
        tDPD       := 150 us;
        tRPH       := 400 ns;
        tRH        := 200 ns;
        tRP        := 200 ns;

    END PROCEDURE TestInit;

    ---------------------------------------------------------------------------
    --Public PROCEDURE to generate command sequence
    ---------------------------------------------------------------------------
    PROCEDURE   Generate_TC
         (  Model       : IN  STRING  ;
            Series      : IN  NATURAL RANGE 1 TO 40;
            TestCase    : IN  NATURAL RANGE 1 TO 15;
            command_seq : OUT cmd_seq_type
         )
    IS
    BEGIN
        FOR i IN 1 TO 600 LOOP
            command_seq(i) :=(done,0,0,0,0,0,0,valid,0 ns);
        END LOOP;

        REPORT "------------------------------------------------------" ;
        REPORT "------------------------------------------------------" ;
        REPORT "TestSeries : "& to_int_str(Series )&"   "&
               "TC         : "& to_int_str(TestCase);
        REPORT "------------------------------------------------------" ;
        CASE Series IS

            WHEN 1  =>    --powerup negative test
                REPORT "POWERUP negative test";
                    --cmd,data_num,data_hi,data_lo,ubm,lbm,addr,aux,time
                command_seq(1) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                command_seq(2) :=(wt      ,0,0,0,0,0,0,valid,50 us);
                --During POR the device can not be selected, will not accept
                --commands, and does not drive outputs.
                --Verify that command is ignored, output is not driven
                command_seq(3) :=(rd_HiZ,1,0,0,0,0,0,valid,0 ns);
                command_seq(4) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                command_seq(5) :=(wt      ,0,0,0,0,0,0,valid,tVCS - 50 us);
                command_seq(6) :=(h_reset ,0,0,0,0,0,0,valid,tRP);
                --During HW Reset the device can not be selected, will not accept
                --commands, and does not drive outputs.
                --Verify that command is ignored, output is not driven
                command_seq(7) :=(rd_HiZ,1,0,0,0,0,0,valid,0 ns);
                command_seq(8) :=(wt      ,0,0,0,0,0,0,valid,(tRP+tRH));
                command_seq(9) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                command_seq(10):=(done    ,0,0,0,0,0,0,valid,0 ns);

            WHEN 2  =>
                CASE Testcase IS
                    WHEN 1 =>
                    REPORT "Positive, continuous burst read";
                    --cmd,data_num,data_hi,data_lo,ubm,lbm,addr,aux,time
                    command_seq(1) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- Write to Config Registar to set burst read/write parameters
                    -- Read Latency = 4;
                    command_seq(2) :=(wr_reg,1,16#8F#,16#FF#,0,0,0,valid,0 ns);
                    command_seq(3) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(4) :=(rd_mem_cont,4,0,0,0,0,3,valid,0 ns);
                    command_seq(5) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(6) :=(rd_mem_cont,10,0,0,0,0,16#003000#,valid,0 ns);
                    command_seq(7) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- read around the Mem size
                    command_seq(8) :=(rd_mem_cont,10,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(9) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- load CR
                    -- RL = 4; Variable latency
                    command_seq(10) :=(wr_reg,1,16#8F#,16#F7#,0,0,0,valid,0 ns);
                    command_seq(11) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(12) :=(rd_mem_cont,4,0,0,0,0,16#002000#,valid,0 ns);
                    -- HW reset
                    command_seq(13) :=(h_reset ,0,0,0,0,0,0,valid,tRP);
                    command_seq(14) :=(wt      ,0,0,0,0,0,0,valid,(tRP+tRH));
                    -- CR will get default value
                    command_seq(15) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(16) :=(rd_reg,1,0,0,0,0,0,valid,0 ns);
                    command_seq(17) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(18):=(done    ,0,0,0,0,0,0,valid,0 ns);

                WHEN 2 =>
                    REPORT "Positive, wrapped burst read with legacy sequencing";
                    --cmd,data_num,data_hi,data_lo,ubm,lbm,addr,aux,time
                    command_seq(1) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- CR=8F1Fh
                    -- BL=16W, RL=6, Hybrid burst disabled, Fixed latency
                    command_seq(2) :=(rd_mem_wrap,20,0,0,0,0,3,valid,0 ns);
                    command_seq(3) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(4) :=(rd_mem_wrap,20,0,0,0,0,35,valid,0 ns);
                    command_seq(5) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=8W, RL=4, Hybrid burst disabled, Fixed latency
                    command_seq(6) :=(wr_reg,1,16#8F#,16#FE#,0,0,0,valid,0 ns);
                    command_seq(7) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(8) :=(rd_mem_wrap,12,0,0,0,0,7,valid,0 ns);
                    command_seq(9) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(10) :=(rd_mem_wrap,12,0,0,0,0,24,valid,0 ns);
                    command_seq(11) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(12) :=(rd_mem_wrap,10,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(13) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (legacy), RL=4, Hybrid burst disabled, Fixed latency
                    command_seq(14) :=(wr_reg,1,16#8F#,16#FC#,0,0,0,valid,0 ns);
                    command_seq(15) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(16) :=(rd_mem_wrap,35,0,0,0,0,7,valid,0 ns);
                    command_seq(17) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(18) :=(rd_mem_wrap,35,0,0,0,0,34,valid,0 ns);
                    command_seq(19) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(20) :=(rd_mem_wrap,35,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(21) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (alternate), RL=4, Hybrid burst disabled, Fixed latency
                    command_seq(22) :=(wr_reg,1,16#8F#,16#FD#,0,0,0,valid,0 ns);
                    command_seq(23) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(24) :=(rd_mem_wrap,35,0,0,0,0,7,valid,0 ns);
                    command_seq(25) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(26) :=(rd_mem_wrap,35,0,0,0,0,24,valid,0 ns);
                    command_seq(27) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(28) :=(rd_mem_wrap,35,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(29) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    --Variable latency
                    -- Load CR
                    -- BL=8W, RL=4, Hybrid burst disabled, Variable latency
                    command_seq(30) :=(wr_reg,1,16#8F#,16#F6#,0,0,0,valid,0 ns);
                    command_seq(31) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(32) :=(rd_mem_wrap,12,0,0,0,0,7,valid,0 ns);
                    command_seq(33) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=16W, RL=4, Hybrid burst disabled, Variable latency
                    command_seq(34) :=(wr_reg,1,16#8F#,16#F7#,0,0,0,valid,0 ns);
                    command_seq(35) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(36) :=(rd_mem_wrap,20,0,0,0,0,16,valid,0 ns);
                    command_seq(37) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (legacy), RL=4, Hybrid burst disabled, Variable latency
                    command_seq(38) :=(wr_reg,1,16#8F#,16#F4#,0,0,0,valid,0 ns);
                    command_seq(39) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(40) :=(rd_mem_wrap,33,0,0,0,0,27,valid,0 ns);
                    command_seq(41) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (alternate), RL=4, Hybrid burst disabled, Variable latency
                    command_seq(42) :=(wr_reg,1,16#8F#,16#F5#,0,0,0,valid,0 ns);
                    command_seq(43) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(44) :=(rd_mem_wrap,33,0,0,0,0,7,valid,0 ns);
                    command_seq(45) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- HW reset
                    command_seq(46) :=(h_reset ,0,0,0,0,0,0,valid,tRP);
                    command_seq(47) :=(wt      ,0,0,0,0,0,0,valid,(tRP+tRH));
                    -- CR will get default value
                    command_seq(48) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(49) :=(rd_reg,1,0,0,0,0,0,valid,0 ns);
                    command_seq(50) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(51) :=(done    ,0,0,0,0,0,0,valid,0 ns);

                WHEN 3 =>
                    REPORT "Positive, Hybrid Burst read ";
                    --cmd,data_num,data_hi,data_lo,ubm,lbm,addr,aux,time
                    command_seq(1) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=8W, RL=4, Hybrid burst enabled, Fixed latency
                    command_seq(2) :=(wr_reg,1,16#8F#,16#FA#,0,0,0,valid,0 ns);
                    command_seq(3) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(4) :=(rd_mem_wrap,12,0,0,0,0,7,valid,0 ns);
                    command_seq(5) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(6) :=(rd_mem_wrap,12,0,0,0,0,24,valid,0 ns);
                    command_seq(7) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(8) :=(rd_mem_wrap,12,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(9) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=16W, RL=4, Hybrid burst enable, Fixed latency
                    command_seq(10) :=(wr_reg,1,16#8F#,16#FB#,0,0,0,valid,0 ns);

                    command_seq(11) :=(rd_mem_wrap,20,0,0,0,0,3,valid,0 ns);
                    command_seq(12) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(13) :=(rd_mem_wrap,20,0,0,0,0,25,valid,0 ns);
                    command_seq(14) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(15) :=(rd_mem_wrap,20,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(16) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (legacy), RL=4, Hybrid burst enable, Fixed latency
                    command_seq(17) :=(wr_reg,1,16#8F#,16#F8#,0,0,0,valid,0 ns);
                    command_seq(18) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(19) :=(rd_mem_wrap,35,0,0,0,0,7,valid,0 ns);
                    command_seq(20) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(21) :=(rd_mem_wrap,35,0,0,0,0,34,valid,0 ns);
                    command_seq(22) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(23) :=(rd_mem_wrap,35,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(24) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (alternate), RL=4, Hybrid burst enabled, Fixed latency
                    command_seq(25) :=(wr_reg,1,16#8F#,16#F9#,0,0,0,valid,0 ns);
                    command_seq(26) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(27) :=(rd_mem_wrap,35,0,0,0,0,7,valid,0 ns);
                    command_seq(28) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(29) :=(rd_mem_wrap,35,0,0,0,0,24,valid,0 ns);
                    command_seq(30) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(31) :=(rd_mem_wrap,45,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(32) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(33) :=(done    ,0,0,0,0,0,0,valid,0 ns);

                    WHEN OTHERS  =>  null;
                END CASE;

            WHEN 3  =>
                CASE Testcase IS
                    WHEN 1 =>
                    REPORT "Positive, continuous burst write";
                    --cmd,data_num,data_hi,data_lo,ubm,lbm,addr,aux,time
                    command_seq(1) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- Write to Config Registar to set burst read/write parameters
                    -- RL=4;
                    command_seq(2) :=(wr_reg,1,16#8F#,16#FE#,0,0,0,valid,0 ns);
                    command_seq(3) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(4) :=(wr_mem_cont,0,0,0,0,0,16#5000#,valid,0 ns);
                    command_seq(5) :=(w_data_cont,0,16#AA#,16#BB#,0,0,0,valid,0 ns);
                    command_seq(6) :=(w_data_cont,0,16#CC#,16#DD#,0,1,0,valid,0 ns);
                    command_seq(7) :=(w_data_cont,0,16#EE#,16#FF#,1,0,0,valid,0 ns);
                    command_seq(8) :=(w_data_cont,0,16#00#,16#11#,1,1,0,valid,0 ns);
                    command_seq(9) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(10) :=(rd_mem_cont,4,0,0,0,0,16#5000#,valid,0 ns);
                    command_seq(11) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(12) :=(wr_mem_cont,0,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(13) :=(w_data_cont,0,16#11#,16#22#,0,0,0,valid,0 ns);
                    command_seq(14) :=(w_data_cont,0,16#33#,16#44#,0,1,0,valid,0 ns);
                    command_seq(15) :=(w_data_cont,0,16#55#,16#66#,1,0,0,valid,0 ns);
                    command_seq(16) :=(w_data_cont,0,16#77#,16#88#,1,1,0,valid,0 ns);
                    command_seq(17) :=(w_data_cont,0,16#99#,16#AA#,0,0,0,valid,0 ns);
                    command_seq(18) :=(w_data_cont,0,16#BB#,16#CC#,0,1,0,valid,0 ns);
                    command_seq(19) :=(w_data_cont,0,16#DD#,16#EE#,1,0,0,valid,0 ns);
                    command_seq(20) :=(w_data_cont,0,16#FF#,16#00#,1,1,0,valid,0 ns);
                    command_seq(21) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(22) :=(rd_mem_cont,10,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(23) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- load CR
                    -- RL = 4; Variable latency
                    command_seq(24) :=(wr_reg,1,16#8F#,16#F7#,0,0,0,valid,0 ns);
                    command_seq(25) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(26) :=(wr_mem_cont,4,0,0,0,0,16#A000#,valid,0 ns);
                    command_seq(27) :=(w_data_cont,0,16#77#,16#88#,0,0,0,valid,0 ns);
                    command_seq(28) :=(w_data_cont,0,16#03#,16#30#,0,1,0,valid,0 ns);
                    command_seq(29) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(30) :=(rd_mem_cont,3,0,0,0,0,16#A000#,valid,0 ns);
                    command_seq(31) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- HW reset
                    command_seq(32) :=(h_reset ,0,0,0,0,0,0,valid,tRP);
                    command_seq(33) :=(wt      ,0,0,0,0,0,0,valid,(tRP+tRH));
                    -- CR gets default value
                    command_seq(34) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(35) :=(rd_reg,1,0,0,0,0,0,valid,0 ns);
                    command_seq(36) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(37):=(done    ,0,0,0,0,0,0,valid,0 ns);

                    WHEN 2 =>
                    REPORT "Positive, wrapped burst write with legacy sequencing";
                    --cmd,data_num,data_hi,data_lo,ubm,lbm,addr,aux,time
                    command_seq(1) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- CR=8F1Fh
                    -- BL=16W, RL=6, Hybrid burst disabled, Fixed latency
                    command_seq(2) :=(wr_mem_wrap,0,0,0,0,0,16#30003#,valid,0 ns);
                    command_seq(3) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,valid,0 ns);
                    command_seq(4) :=(w_data_wrap,0,16#00#,16#01#,0,0,0,valid,0 ns);
                    command_seq(5) :=(w_data_wrap,0,16#00#,16#02#,0,0,0,valid,0 ns);
                    command_seq(6) :=(w_data_wrap,0,16#00#,16#03#,0,0,0,valid,0 ns);
                    command_seq(7) :=(w_data_wrap,0,16#00#,16#04#,0,0,0,valid,0 ns);
                    command_seq(8) :=(w_data_wrap,0,16#00#,16#05#,0,0,0,valid,0 ns);
                    command_seq(9) :=(w_data_wrap,0,16#00#,16#06#,0,0,0,valid,0 ns);
                    command_seq(10) :=(w_data_wrap,0,16#00#,16#07#,0,0,0,valid,0 ns);
                    command_seq(11) :=(w_data_wrap,0,16#00#,16#08#,0,0,0,valid,0 ns);
                    command_seq(12) :=(w_data_wrap,0,16#00#,16#09#,0,0,0,valid,0 ns);
                    command_seq(13) :=(w_data_wrap,0,16#00#,16#0A#,0,0,0,valid,0 ns);
                    command_seq(14) :=(w_data_wrap,0,16#00#,16#0B#,0,0,0,valid,0 ns);
                    command_seq(15) :=(w_data_wrap,0,16#00#,16#0C#,0,0,0,valid,0 ns);
                    command_seq(16) :=(w_data_wrap,0,16#00#,16#0D#,0,0,0,valid,0 ns);
                    command_seq(17) :=(w_data_wrap,0,16#00#,16#0E#,0,0,0,valid,0 ns);
                    command_seq(18) :=(w_data_wrap,0,16#00#,16#0F#,0,0,0,valid,0 ns);
                    command_seq(19) :=(w_data_wrap,0,16#11#,16#11#,0,0,0,valid,0 ns);
                    command_seq(20) :=(w_data_wrap,0,16#22#,16#22#,0,0,0,valid,0 ns);
                    command_seq(21) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(22) :=(rd_mem_wrap,20,0,0,0,0,16#30003#,valid,0 ns);
                    command_seq(23) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=8W, RL=4, Hybrid burst disabled, Fixed latency
                    command_seq(24) :=(wr_reg,1,16#8F#,16#FE#,0,0,0,valid,0 ns);
                    command_seq(25) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(26) :=(wr_mem_wrap,0,0,0,0,0,16#40003#,valid,0 ns);
                    command_seq(27) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,valid,0 ns);
                    command_seq(28) :=(w_data_wrap,0,16#00#,16#01#,0,0,0,valid,0 ns);
                    command_seq(29) :=(w_data_wrap,0,16#00#,16#02#,0,0,0,valid,0 ns);
                    command_seq(30) :=(w_data_wrap,0,16#00#,16#03#,0,0,0,valid,0 ns);
                    command_seq(31) :=(w_data_wrap,0,16#00#,16#04#,0,0,0,valid,0 ns);
                    command_seq(32) :=(w_data_wrap,0,16#00#,16#05#,0,0,0,valid,0 ns);
                    command_seq(33) :=(w_data_wrap,0,16#00#,16#06#,0,0,0,valid,0 ns);
                    command_seq(34) :=(w_data_wrap,0,16#00#,16#07#,0,0,0,valid,0 ns);
                    command_seq(35) :=(w_data_wrap,0,16#11#,16#11#,0,0,0,valid,0 ns);
                    command_seq(36) :=(w_data_wrap,0,16#22#,16#22#,0,0,0,valid,0 ns);
                    command_seq(37) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(38) :=(rd_mem_wrap,11,0,0,0,0,16#40003#,valid,0 ns);
                    command_seq(39) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(40) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(41) :=(wr_mem_wrap,0,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(42) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,valid,0 ns);
                    command_seq(43) :=(w_data_wrap,0,16#00#,16#01#,0,0,0,valid,0 ns);
                    command_seq(44) :=(w_data_wrap,0,16#00#,16#02#,0,0,0,valid,0 ns);
                    command_seq(45) :=(w_data_wrap,0,16#00#,16#03#,0,0,0,valid,0 ns);
                    command_seq(46) :=(w_data_wrap,0,16#00#,16#04#,0,0,0,valid,0 ns);
                    command_seq(47) :=(w_data_wrap,0,16#00#,16#05#,0,0,0,valid,0 ns);
                    command_seq(48) :=(w_data_wrap,0,16#00#,16#06#,0,0,0,valid,0 ns);
                    command_seq(49) :=(w_data_wrap,0,16#00#,16#07#,0,0,0,valid,0 ns);
                    command_seq(50) :=(w_data_wrap,0,16#11#,16#11#,0,0,0,valid,0 ns);
                    command_seq(51) :=(w_data_wrap,0,16#22#,16#22#,0,0,0,valid,0 ns);
                    command_seq(52) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(53) :=(rd_mem_wrap,11,0,0,0,0,16#3FFFFA#,valid,0 ns);
                    command_seq(54) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (legacy), RL=4, Hybrid burst disabled, Fixed latency
                    command_seq(55) :=(wr_reg,1,16#8F#,16#FC#,0,0,0,valid,0 ns);
                    command_seq(56) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(57) :=(wr_mem_wrap,0,0,0,0,0,16#40003#,valid,0 ns);
                    command_seq(58) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,valid,0 ns);
                    command_seq(59) :=(w_data_wrap,0,16#00#,16#01#,0,0,0,valid,0 ns);
                    command_seq(60) :=(w_data_wrap,0,16#00#,16#02#,0,0,0,valid,0 ns);
                    command_seq(61) :=(w_data_wrap,0,16#00#,16#03#,0,0,0,valid,0 ns);
                    command_seq(62) :=(w_data_wrap,0,16#00#,16#04#,0,0,0,valid,0 ns);
                    command_seq(63) :=(w_data_wrap,0,16#00#,16#05#,0,0,0,valid,0 ns);
                    command_seq(64) :=(w_data_wrap,0,16#00#,16#06#,0,0,0,valid,0 ns);
                    command_seq(65) :=(w_data_wrap,0,16#00#,16#07#,0,0,0,valid,0 ns);
                    command_seq(66) :=(w_data_wrap,0,16#00#,16#08#,0,0,0,valid,0 ns);
                    command_seq(67) :=(w_data_wrap,0,16#00#,16#09#,0,0,0,valid,0 ns);
                    command_seq(68) :=(w_data_wrap,0,16#00#,16#0A#,0,0,0,valid,0 ns);
                    command_seq(69) :=(w_data_wrap,0,16#00#,16#0B#,0,0,0,valid,0 ns);
                    command_seq(70) :=(w_data_wrap,0,16#00#,16#0C#,0,0,0,valid,0 ns);
                    command_seq(71) :=(w_data_wrap,0,16#00#,16#0D#,0,0,0,valid,0 ns);
                    command_seq(72) :=(w_data_wrap,0,16#00#,16#0E#,0,0,0,valid,0 ns);
                    command_seq(73) :=(w_data_wrap,0,16#00#,16#0F#,0,0,0,valid,0 ns);
                    command_seq(74) :=(w_data_wrap,0,16#00#,16#10#,0,0,0,valid,0 ns);
                    command_seq(75) :=(w_data_wrap,0,16#00#,16#11#,0,0,0,valid,0 ns);
                    command_seq(76) :=(w_data_wrap,0,16#00#,16#12#,0,0,0,valid,0 ns);
                    command_seq(77) :=(w_data_wrap,0,16#00#,16#13#,0,0,0,valid,0 ns);
                    command_seq(78) :=(w_data_wrap,0,16#00#,16#14#,0,0,0,valid,0 ns);
                    command_seq(79) :=(w_data_wrap,0,16#00#,16#15#,0,0,0,valid,0 ns);
                    command_seq(80) :=(w_data_wrap,0,16#00#,16#16#,0,0,0,valid,0 ns);
                    command_seq(81) :=(w_data_wrap,0,16#00#,16#17#,0,0,0,valid,0 ns);
                    command_seq(82) :=(w_data_wrap,0,16#00#,16#18#,0,0,0,valid,0 ns);
                    command_seq(83) :=(w_data_wrap,0,16#00#,16#19#,0,0,0,valid,0 ns);
                    command_seq(84) :=(w_data_wrap,0,16#00#,16#1A#,0,0,0,valid,0 ns);
                    command_seq(85) :=(w_data_wrap,0,16#00#,16#1B#,0,0,0,valid,0 ns);
                    command_seq(86) :=(w_data_wrap,0,16#00#,16#1C#,0,0,0,valid,0 ns);
                    command_seq(87) :=(w_data_wrap,0,16#00#,16#1D#,0,0,0,valid,0 ns);
                    command_seq(88) :=(w_data_wrap,0,16#00#,16#1E#,0,0,0,valid,0 ns);
                    command_seq(89) :=(w_data_wrap,0,16#00#,16#1F#,0,0,0,valid,0 ns);
                    command_seq(90) :=(w_data_wrap,0,16#11#,16#11#,0,0,0,valid,0 ns);
                    command_seq(91) :=(w_data_wrap,0,16#22#,16#22#,0,0,0,valid,0 ns);
                    command_seq(92) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(93) :=(rd_mem_wrap,35,0,0,0,0,16#40003#,valid,0 ns);
                    command_seq(94) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (alternate), RL=4, Hybrid burst disable, Fixed latency
                    command_seq(95) :=(wr_reg,1,16#8F#,16#FD#,0,0,0,valid,0 ns);
                    command_seq(96) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(97) :=(wr_mem_wrap,0,0,0,0,0,16#50003#,valid,0 ns);
                    command_seq(98) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,valid,0 ns);
                    command_seq(99) :=(w_data_wrap,0,16#00#,16#01#,0,0,0,valid,0 ns);
                    command_seq(100) :=(w_data_wrap,0,16#00#,16#02#,0,0,0,valid,0 ns);
                    command_seq(101) :=(w_data_wrap,0,16#00#,16#03#,0,0,0,valid,0 ns);
                    command_seq(102) :=(w_data_wrap,0,16#00#,16#04#,0,0,0,valid,0 ns);
                    command_seq(103) :=(w_data_wrap,0,16#00#,16#05#,0,0,0,valid,0 ns);
                    command_seq(104) :=(w_data_wrap,0,16#00#,16#06#,0,0,0,valid,0 ns);
                    command_seq(105) :=(w_data_wrap,0,16#00#,16#07#,0,0,0,valid,0 ns);
                    command_seq(106) :=(w_data_wrap,0,16#00#,16#08#,0,0,0,valid,0 ns);
                    command_seq(107) :=(w_data_wrap,0,16#00#,16#09#,0,0,0,valid,0 ns);
                    command_seq(108) :=(w_data_wrap,0,16#00#,16#0A#,0,0,0,valid,0 ns);
                    command_seq(109) :=(w_data_wrap,0,16#00#,16#0B#,0,0,0,valid,0 ns);
                    command_seq(110) :=(w_data_wrap,0,16#00#,16#0C#,0,0,0,valid,0 ns);
                    command_seq(111) :=(w_data_wrap,0,16#00#,16#0D#,0,0,0,valid,0 ns);
                    command_seq(112) :=(w_data_wrap,0,16#00#,16#0E#,0,0,0,valid,0 ns);
                    command_seq(113) :=(w_data_wrap,0,16#00#,16#0F#,0,0,0,valid,0 ns);
                    command_seq(114) :=(w_data_wrap,0,16#00#,16#10#,0,0,0,valid,0 ns);
                    command_seq(115) :=(w_data_wrap,0,16#00#,16#11#,0,0,0,valid,0 ns);
                    command_seq(116) :=(w_data_wrap,0,16#00#,16#12#,0,0,0,valid,0 ns);
                    command_seq(117) :=(w_data_wrap,0,16#00#,16#13#,0,0,0,valid,0 ns);
                    command_seq(118) :=(w_data_wrap,0,16#00#,16#14#,0,0,0,valid,0 ns);
                    command_seq(119) :=(w_data_wrap,0,16#00#,16#15#,0,0,0,valid,0 ns);
                    command_seq(120) :=(w_data_wrap,0,16#00#,16#16#,0,0,0,valid,0 ns);
                    command_seq(121) :=(w_data_wrap,0,16#00#,16#17#,0,0,0,valid,0 ns);
                    command_seq(122) :=(w_data_wrap,0,16#00#,16#18#,0,0,0,valid,0 ns);
                    command_seq(123) :=(w_data_wrap,0,16#00#,16#19#,0,0,0,valid,0 ns);
                    command_seq(124) :=(w_data_wrap,0,16#00#,16#1A#,0,0,0,valid,0 ns);
                    command_seq(125) :=(w_data_wrap,0,16#00#,16#1B#,0,0,0,valid,0 ns);
                    command_seq(126) :=(w_data_wrap,0,16#00#,16#1C#,0,0,0,valid,0 ns);
                    command_seq(127) :=(w_data_wrap,0,16#00#,16#1D#,0,0,0,valid,0 ns);
                    command_seq(128) :=(w_data_wrap,0,16#00#,16#1E#,0,0,0,valid,0 ns);
                    command_seq(129) :=(w_data_wrap,0,16#00#,16#1F#,0,0,0,valid,0 ns);
                    command_seq(130) :=(w_data_wrap,0,16#11#,16#11#,0,0,0,valid,0 ns);
                    command_seq(131) :=(w_data_wrap,0,16#22#,16#22#,0,0,0,valid,0 ns);
                    command_seq(132) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(133) :=(rd_mem_wrap,35,0,0,0,0,16#50003#,valid,0 ns);
                    command_seq(134) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(135) :=(rd_mem_wrap,35,0,0,0,0,16#50000#,valid,0 ns);
                    command_seq(136) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(137) :=(rd_mem_wrap,35,0,0,0,0,16#50010#,valid,0 ns);
                    command_seq(138) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(139):=(done    ,0,0,0,0,0,0,valid,0 ns);

                    WHEN 3 =>
                    REPORT "Positive, Hybrid Burst write ";
                    --cmd,data_num,data_hi,data_lo,ubm,lbm,addr,aux,time
                    command_seq(1) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=8W, RL=4, Hybrid burst enabled, Fixed latency
                    command_seq(2) :=(wr_reg,1,16#8F#,16#FA#,0,0,0,valid,0 ns);
                    command_seq(3) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(4) :=(wr_mem_wrap,0,0,0,0,0,16#60005#,valid,0 ns);
                    command_seq(5) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,valid,0 ns);
                    command_seq(6) :=(w_data_wrap,0,16#00#,16#01#,0,0,0,valid,0 ns);
                    command_seq(7) :=(w_data_wrap,0,16#00#,16#02#,0,0,0,valid,0 ns);
                    command_seq(8) :=(w_data_wrap,0,16#00#,16#03#,0,0,0,valid,0 ns);
                    command_seq(9) :=(w_data_wrap,0,16#00#,16#04#,0,0,0,valid,0 ns);
                    command_seq(10) :=(w_data_wrap,0,16#00#,16#05#,0,0,0,valid,0 ns);
                    command_seq(11) :=(w_data_wrap,0,16#00#,16#06#,0,0,0,valid,0 ns);
                    command_seq(12) :=(w_data_wrap,0,16#00#,16#07#,0,0,0,valid,0 ns);
                    command_seq(13) :=(w_data_wrap,0,16#11#,16#11#,0,0,0,valid,0 ns);
                    command_seq(14) :=(w_data_wrap,0,16#22#,16#22#,0,0,0,valid,0 ns);
                    command_seq(15) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(16) :=(rd_mem_wrap,11,0,0,0,0,16#60005#,valid,0 ns);
                    command_seq(17) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(18) :=(rd_mem_wrap,11,0,0,0,0,16#60000#,valid,0 ns);
                    command_seq(19) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(20) :=(rd_mem_wrap,11,0,0,0,0,16#60007#,valid,0 ns);
                    command_seq(21) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=16W, RL=4, Hybrid burst enabled, Fixed latency
                    command_seq(22) :=(wr_reg,1,16#8F#,16#FB#,0,0,0,valid,0 ns);
                    command_seq(23) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(24) :=(wr_mem_wrap,0,0,0,0,0,16#70005#,valid,0 ns);
                    command_seq(25) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,valid,0 ns);
                    command_seq(26) :=(w_data_wrap,0,16#00#,16#01#,0,0,0,valid,0 ns);
                    command_seq(27) :=(w_data_wrap,0,16#00#,16#02#,0,0,0,valid,0 ns);
                    command_seq(28) :=(w_data_wrap,0,16#00#,16#03#,0,0,0,valid,0 ns);
                    command_seq(29) :=(w_data_wrap,0,16#00#,16#04#,0,0,0,valid,0 ns);
                    command_seq(30) :=(w_data_wrap,0,16#00#,16#05#,0,0,0,valid,0 ns);
                    command_seq(31) :=(w_data_wrap,0,16#00#,16#06#,0,0,0,valid,0 ns);
                    command_seq(32) :=(w_data_wrap,0,16#00#,16#07#,0,0,0,valid,0 ns);
                    command_seq(33) :=(w_data_wrap,0,16#00#,16#08#,0,0,0,valid,0 ns);
                    command_seq(34) :=(w_data_wrap,0,16#00#,16#09#,0,0,0,valid,0 ns);
                    command_seq(35) :=(w_data_wrap,0,16#00#,16#0A#,0,0,0,valid,0 ns);
                    command_seq(36) :=(w_data_wrap,0,16#00#,16#0B#,0,0,0,valid,0 ns);
                    command_seq(37) :=(w_data_wrap,0,16#00#,16#0C#,0,0,0,valid,0 ns);
                    command_seq(38) :=(w_data_wrap,0,16#00#,16#0D#,0,0,0,valid,0 ns);
                    command_seq(39) :=(w_data_wrap,0,16#00#,16#0E#,0,0,0,valid,0 ns);
                    command_seq(40) :=(w_data_wrap,0,16#00#,16#0F#,0,0,0,valid,0 ns);
                    command_seq(41) :=(w_data_wrap,0,16#11#,16#11#,0,0,0,valid,0 ns);
                    command_seq(42) :=(w_data_wrap,0,16#22#,16#22#,0,0,0,valid,0 ns);
                    command_seq(43) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(44) :=(rd_mem_wrap,20,0,0,0,0,16#70005#,valid,0 ns);
                    command_seq(45) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (legacy), RL=4, Hybrid burst enabled, Fixed latency
                    command_seq(46) :=(wr_reg,1,16#8F#,16#F8#,0,0,0,valid,0 ns);
                    command_seq(47) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(48) :=(wr_mem_wrap,0,0,0,0,0,16#80013#,valid,0 ns);
                    command_seq(49) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,valid,0 ns);
                    command_seq(50) :=(w_data_wrap,0,16#00#,16#01#,0,0,0,valid,0 ns);
                    command_seq(51) :=(w_data_wrap,0,16#00#,16#02#,0,0,0,valid,0 ns);
                    command_seq(52) :=(w_data_wrap,0,16#00#,16#03#,0,0,0,valid,0 ns);
                    command_seq(53) :=(w_data_wrap,0,16#00#,16#04#,0,0,0,valid,0 ns);
                    command_seq(54) :=(w_data_wrap,0,16#00#,16#05#,0,0,0,valid,0 ns);
                    command_seq(55) :=(w_data_wrap,0,16#00#,16#06#,0,0,0,valid,0 ns);
                    command_seq(56) :=(w_data_wrap,0,16#00#,16#07#,0,0,0,valid,0 ns);
                    command_seq(57) :=(w_data_wrap,0,16#00#,16#08#,0,0,0,valid,0 ns);
                    command_seq(58) :=(w_data_wrap,0,16#00#,16#09#,0,0,0,valid,0 ns);
                    command_seq(59) :=(w_data_wrap,0,16#00#,16#0A#,0,0,0,valid,0 ns);
                    command_seq(60) :=(w_data_wrap,0,16#00#,16#0B#,0,0,0,valid,0 ns);
                    command_seq(61) :=(w_data_wrap,0,16#00#,16#0C#,0,0,0,valid,0 ns);
                    command_seq(62) :=(w_data_wrap,0,16#00#,16#0D#,0,0,0,valid,0 ns);
                    command_seq(63) :=(w_data_wrap,0,16#00#,16#0E#,0,0,0,valid,0 ns);
                    command_seq(64) :=(w_data_wrap,0,16#00#,16#0F#,0,0,0,valid,0 ns);
                    command_seq(65) :=(w_data_wrap,0,16#00#,16#10#,0,0,0,valid,0 ns);
                    command_seq(66) :=(w_data_wrap,0,16#00#,16#11#,0,0,0,valid,0 ns);
                    command_seq(67) :=(w_data_wrap,0,16#00#,16#12#,0,0,0,valid,0 ns);
                    command_seq(68) :=(w_data_wrap,0,16#00#,16#13#,0,0,0,valid,0 ns);
                    command_seq(69) :=(w_data_wrap,0,16#00#,16#14#,0,0,0,valid,0 ns);
                    command_seq(70) :=(w_data_wrap,0,16#00#,16#15#,0,0,0,valid,0 ns);
                    command_seq(71) :=(w_data_wrap,0,16#00#,16#16#,0,0,0,valid,0 ns);
                    command_seq(72) :=(w_data_wrap,0,16#00#,16#17#,0,0,0,valid,0 ns);
                    command_seq(73) :=(w_data_wrap,0,16#00#,16#18#,0,0,0,valid,0 ns);
                    command_seq(74) :=(w_data_wrap,0,16#00#,16#19#,0,0,0,valid,0 ns);
                    command_seq(75) :=(w_data_wrap,0,16#00#,16#1A#,0,0,0,valid,0 ns);
                    command_seq(76) :=(w_data_wrap,0,16#00#,16#1B#,0,0,0,valid,0 ns);
                    command_seq(77) :=(w_data_wrap,0,16#00#,16#1C#,0,0,0,valid,0 ns);
                    command_seq(78) :=(w_data_wrap,0,16#00#,16#1D#,0,0,0,valid,0 ns);
                    command_seq(79) :=(w_data_wrap,0,16#00#,16#1E#,0,0,0,valid,0 ns);
                    command_seq(80) :=(w_data_wrap,0,16#00#,16#1F#,0,0,0,valid,0 ns);
                    command_seq(81) :=(w_data_wrap,0,16#11#,16#11#,0,0,0,valid,0 ns);
                    command_seq(82) :=(w_data_wrap,0,16#22#,16#22#,0,0,0,valid,0 ns);
                    command_seq(83) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(84) :=(rd_mem_wrap,35,0,0,0,0,16#80013#,valid,0 ns);
                    command_seq(85) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    -- Load CR
                    -- BL=32W (alternate), RL=4, Hybrid burst enabled, Fixed latency
                    command_seq(86) :=(wr_reg,1,16#8F#,16#F9#,0,0,0,valid,0 ns);
                    command_seq(87) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(88) :=(wr_mem_wrap,0,0,0,0,0,16#90013#,valid,0 ns);
                    command_seq(89) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,valid,0 ns);
                    command_seq(90) :=(w_data_wrap,0,16#00#,16#01#,0,0,0,valid,0 ns);
                    command_seq(91) :=(w_data_wrap,0,16#00#,16#02#,0,0,0,valid,0 ns);
                    command_seq(92) :=(w_data_wrap,0,16#00#,16#03#,0,0,0,valid,0 ns);
                    command_seq(93) :=(w_data_wrap,0,16#00#,16#04#,0,0,0,valid,0 ns);
                    command_seq(94) :=(w_data_wrap,0,16#00#,16#05#,0,0,0,valid,0 ns);
                    command_seq(95) :=(w_data_wrap,0,16#00#,16#06#,0,0,0,valid,0 ns);
                    command_seq(96) :=(w_data_wrap,0,16#00#,16#07#,0,0,0,valid,0 ns);
                    command_seq(97) :=(w_data_wrap,0,16#00#,16#08#,0,0,0,valid,0 ns);
                    command_seq(98) :=(w_data_wrap,0,16#00#,16#09#,0,0,0,valid,0 ns);
                    command_seq(99) :=(w_data_wrap,0,16#00#,16#0A#,0,0,0,valid,0 ns);
                    command_seq(100) :=(w_data_wrap,0,16#00#,16#0B#,0,0,0,valid,0 ns);
                    command_seq(101) :=(w_data_wrap,0,16#00#,16#0C#,0,0,0,valid,0 ns);
                    command_seq(102) :=(w_data_wrap,0,16#00#,16#0D#,0,0,0,valid,0 ns);
                    command_seq(103) :=(w_data_wrap,0,16#00#,16#0E#,0,0,0,valid,0 ns);
                    command_seq(104) :=(w_data_wrap,0,16#00#,16#0F#,0,0,0,valid,0 ns);
                    command_seq(105) :=(w_data_wrap,0,16#00#,16#10#,0,0,0,valid,0 ns);
                    command_seq(106) :=(w_data_wrap,0,16#00#,16#11#,0,0,0,valid,0 ns);
                    command_seq(107) :=(w_data_wrap,0,16#00#,16#12#,0,0,0,valid,0 ns);
                    command_seq(108) :=(w_data_wrap,0,16#00#,16#13#,0,0,0,valid,0 ns);
                    command_seq(109) :=(w_data_wrap,0,16#00#,16#14#,0,0,0,valid,0 ns);
                    command_seq(110) :=(w_data_wrap,0,16#00#,16#15#,0,0,0,valid,0 ns);
                    command_seq(111) :=(w_data_wrap,0,16#00#,16#16#,0,0,0,valid,0 ns);
                    command_seq(112) :=(w_data_wrap,0,16#00#,16#17#,0,0,0,valid,0 ns);
                    command_seq(113) :=(w_data_wrap,0,16#00#,16#18#,0,0,0,valid,0 ns);
                    command_seq(114) :=(w_data_wrap,0,16#00#,16#19#,0,0,0,valid,0 ns);
                    command_seq(115) :=(w_data_wrap,0,16#00#,16#1A#,0,0,0,valid,0 ns);
                    command_seq(116) :=(w_data_wrap,0,16#00#,16#1B#,0,0,0,valid,0 ns);
                    command_seq(117) :=(w_data_wrap,0,16#00#,16#1C#,0,0,0,valid,0 ns);
                    command_seq(118) :=(w_data_wrap,0,16#00#,16#1D#,0,0,0,valid,0 ns);
                    command_seq(119) :=(w_data_wrap,0,16#00#,16#1E#,0,0,0,valid,0 ns);
                    command_seq(120) :=(w_data_wrap,0,16#00#,16#1F#,0,0,0,valid,0 ns);
                    command_seq(121) :=(w_data_wrap,0,16#11#,16#11#,0,0,0,valid,0 ns);
                    command_seq(122) :=(w_data_wrap,0,16#22#,16#22#,0,0,0,valid,0 ns);
                    command_seq(123) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(124) :=(rd_mem_wrap,35,0,0,0,0,16#90013#,valid,0 ns);
                    command_seq(125) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(126) :=(rd_mem_wrap,35,0,0,0,0,16#90000#,valid,0 ns);
                    command_seq(127) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(128) :=(rd_mem_wrap,35,0,0,0,0,16#9000F#,valid,0 ns);
                    command_seq(129) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(130) :=(rd_mem_wrap,35,0,0,0,0,16#90010#,valid,0 ns);
                    command_seq(131) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(132) :=(rd_mem_wrap,35,0,0,0,0,16#9001F#,valid,0 ns);
                    command_seq(133) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(134):=(done    ,0,0,0,0,0,0,valid,0 ns);
                   WHEN OTHERS  =>  null;
                END CASE;

            WHEN 4  =>
                CASE Testcase IS
                    WHEN 1 =>
                    REPORT "Positive, Deep Power Down mode";
                    --cmd,data_num,data_hi,data_lo,ubm,lbm,addr,aux,time
                    command_seq(1) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- Enter DPD mode, CR.15 = 0
                    command_seq(2) :=(wr_reg,1,16#0F#,16#1F#,0,0,0,valid,0 ns);
                    command_seq(3) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    --During DPD mode the device can not be selected
                    command_seq(4) :=(wr_mem_wrap,0,0,0,0,0,16#30003#,violate,0 ns);
                    command_seq(5) :=(w_data_wrap,0,16#00#,16#00#,0,0,0,violate,0 ns);
                    command_seq(6) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(7) :=(rd_HiZ,1,0,0,0,0,16#30003#,valid,0 ns);
                    command_seq(8) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(9) :=(wr_reg,1,16#8F#,16#18#,0,0,0,valid,0 ns);
                    command_seq(10) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(11) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- Wait for DPD wake-up period
                    command_seq(12) :=(wt      ,0,0,0,0,0,0,valid,tDPD);
                    -- verify that write operation attempted during DPD mode is not performed
                    command_seq(13) :=(rd_reg,0,0,0,0,0,0,valid,0 ns);
                    command_seq(14) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(15) :=(rd_HiZ,1,0,0,0,0,16#30003#,valid,0 ns);
                    command_seq(16) :=(idle    ,0,0,0,0,0,0,valid,0 ns);

                    command_seq(17) :=(wr_reg,1,16#8F#,16#1F#,0,0,0,valid,0 ns);
                    command_seq(18) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    -- enter DPD mode
                    command_seq(19) :=(wr_reg,1,16#0F#,16#1F#,0,0,0,valid,0 ns);
                    command_seq(20) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(21) :=(wt      ,0,0,0,0,0,0,valid,100 ns);

                    -- HW reset will cause exit DPD mode
                    command_seq(22) :=(h_reset ,0,0,0,0,0,0,valid,202 ns);
                    command_seq(23) :=(wt      ,0,0,0,0,0,0,valid,tRPH+200 ns);
                    -- read mode
                    command_seq(24) :=(rd_mem_cont,5,0,0,0,0,0,valid,0 ns);
                    command_seq(25) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(26) :=(rd_reg,0,0,0,0,0,0,valid,0 ns);
                    command_seq(27) :=(idle    ,0,0,0,0,0,0,valid,0 ns);
                    command_seq(28) :=(done    ,0,0,0,0,0,0,valid,0 ns);

                   WHEN OTHERS  =>  null;
                END CASE;


            WHEN OTHERS => NULL;
        END CASE;
    REPORT "------------------------------------------------------";
END PROCEDURE Generate_TC;

    ---------------------------------------------------------------------------
    -- PUBLIC
    -- CHECKER PROCEDURES
    ---------------------------------------------------------------------------

 PROCEDURE   Check_Z (
        DQ   :  IN std_logic_vector(7 downto 0);
        SIGNAL check_err:  OUT std_logic) IS
    BEGIN
        ASSERT DQ(7 downto 0)="ZZZZZZZZ"
            REPORT "output should be HiZ"
            SEVERITY error;

        IF DQ(7 downto 0)/="ZZZZZZZZ" THEN
            check_err <= '1';
        ELSE
            check_err <= '0';
        END IF;

        ASSERT DQ(7 downto 0)/="ZZZZZZZZ"
            REPORT "Read OK - output HiZ"
            SEVERITY note;
    END PROCEDURE Check_Z;

----- Check read from memory
    PROCEDURE   Check_read (
        DQ    :  IN std_logic_vector(15 downto 0);
        Data1  :  IN NATURAL;
        Data2  :  IN NATURAL;
        SIGNAL check_err:  OUT std_logic) IS
    BEGIN
        ASSERT to_nat(DQ(7 downto 0)) = Data1
            REPORT "READ: expected data =" &
            to_hex_str(Data1)&" got " &
            to_hex_str(DQ(7 downto 0))
            SEVERITY error;

        ASSERT to_nat(DQ(15 downto 8)) = Data2
            REPORT "READ: expected data =" &
            to_hex_str(Data2)&" got " &
            to_hex_str(DQ(15 downto 8))
            SEVERITY error;

        ASSERT to_nat(DQ(7 downto 0))/=Data1
            REPORT "READ: OK - "&
            to_hex_str(DQ(7 downto 0))
            SEVERITY note;

        ASSERT to_nat(DQ(15 downto 8))/=Data2
            REPORT "READ: OK - "&
            to_hex_str(DQ(15 downto 8))
            SEVERITY note;

        IF (to_nat(DQ(7 downto 0))/=Data1) OR
            (to_nat(DQ(15 downto 8))/=Data2)THEN
            check_err <= '1';
        ELSE
            check_err <= '0';
        END IF;

    END PROCEDURE Check_read;

    PROCEDURE   Check_RDS (
        RDS        :  IN std_logic;
        RDSValue   :  IN std_logic;
        SIGNAL check_err :  OUT std_logic) IS
    VARIABLE RDScheck : std_logic;
    BEGIN

        IF RDSValue = RDS THEN
            ASSERT FALSE
                REPORT "   RDS signal value : "& to_bin_str(RDS) &
                       " - OK."
                SEVERITY NOTE;
                check_err <= '0';
        ELSE
            ASSERT FALSE
                REPORT "Expected RDS : " & to_bin_str(RDSValue) & " Got : "
                        & to_bin_str(RDS)
                SEVERITY ERROR;
                check_err <= '1';
        END IF;
    END PROCEDURE Check_RDS;

END PACKAGE BODY spansion_tc_pkg;