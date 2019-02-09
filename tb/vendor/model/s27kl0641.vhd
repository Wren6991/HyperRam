--------------------------------------------------------------------------------
--  File name : s27kl0641.vhd
--------------------------------------------------------------------------------
-- Copyright (C) 2015 Spansion, LLC.
--
--  MODIFICATION HISTORY:
--
--  version: |  author:      | mod date: | changes made:
--   V1.0      M.Stojanovic   15 June 17   Initial Release
--   V1.1      M.Stojanovic   16 Mar  01   Changed tDSV time (bug #500 fixed)
--
--------------------------------------------------------------------------------
--  PART DESCRIPTION:
--
--  Library:     Spansion
--  Technology:  RAM
--  Part:        S27KL0641
--
--  Description:   Reduced Pin Count Pseudo Static RAM,
--                 64Mb high-speed CMOS 3.0 Volt Core, x8 data bus
--------------------------------------------------------------------------------
--  Comments :
--      For correct simulation, simulator resolution should be set to 1 ps
--
--
--------------------------------------------------------------------------------
--  Known Bugs:
--
--------------------------------------------------------------------------------
LIBRARY IEEE;
    USE IEEE.std_logic_1164.ALL;
    USE IEEE.VITAL_timing.ALL;
    USE IEEE.VITAL_primitives.ALL;
    USE STD.textio.ALL;

LIBRARY FMF;
    USE FMF.gen_utils.all;
    USE FMF.conversions.all;

-------------------------------------------------------------------------------
-- ENTITY DECLARATION
-------------------------------------------------------------------------------
ENTITY s27kl0641 IS
    GENERIC (
        -- tipd delays: interconnect path delays
        tipd_DQ0             : VitalDelayType01 := VitalZeroDelay01; --
        tipd_DQ1             : VitalDelayType01 := VitalZeroDelay01; --
        tipd_DQ2             : VitalDelayType01 := VitalZeroDelay01; --
        tipd_DQ3             : VitalDelayType01 := VitalZeroDelay01; -- data
        tipd_DQ4             : VitalDelayType01 := VitalZeroDelay01; -- lines
        tipd_DQ5             : VitalDelayType01 := VitalZeroDelay01; --
        tipd_DQ6             : VitalDelayType01 := VitalZeroDelay01; --
        tipd_DQ7             : VitalDelayType01 := VitalZeroDelay01; --

        tipd_CSNeg           : VitalDelayType01 := VitalZeroDelay01; --
        tipd_CK              : VitalDelayType01 := VitalZeroDelay01; --
        tipd_RESETNeg        : VitalDelayType01 := VitalZeroDelay01; --
        tipd_RWDS            : VitalDelayType01 := VitalZeroDelay01; --

        --tpd delays
        tpd_CSNeg_RWDS       : VitalDelayType01Z := UnitDelay01Z;   --tDSZ
        tpd_CK_RWDS          : VitalDelayType01Z := UnitDelay01Z;   --tCKDS

        tpd_CSNeg_DQ0        : VitalDelayType01Z := UnitDelay01Z;   --tOZ
        tpd_CK_DQ0           : VitalDelayType01Z := UnitDelay01Z;   --tCKD

        --tsetup values
        tsetup_CSNeg_CK      : VitalDelayType    := UnitDelay;  --tCSS edge /
        tsetup_DQ0_CK        : VitalDelayType := UnitDelay;  --tIS

        --thold values
        thold_CSNeg_CK       : VitalDelayType := UnitDelay;  --tCSH  edge \
        thold_DQ0_CK         : VitalDelayType := UnitDelay;  --tIH
        thold_CSNeg_RESETNeg : VitalDelayType := UnitDelay; -- tRH

        --trecovery value
        trecovery_CSNeg_CK_posedge_negedge : VitalDelayType := UnitDelay; -- tRWR

        --tpw values: pulse width
        tpw_CK_negedge       : VitalDelayType := UnitDelay; --tCL
        tpw_CK_posedge       : VitalDelayType := UnitDelay; --tCH
        tpw_CSNeg_posedge    : VitalDelayType := UnitDelay; --tCSHI
        tpw_RESETNeg_negedge : VitalDelayType := UnitDelay; --tRP

        --tperiod values
        tperiod_CK      :VitalDelayType := UnitDelay;

        --tdevice values: values for internal delays
        -- power-on reset
        tdevice_VCS              :VitalDelayType := 150 us;
        -- Deep Power Down to Idle wake up time
        tdevice_DPD              :VitalDelayType := 150 us;
        -- Exit Event from Deep Power Down
        tdevice_DPDCSL           :VitalDelayType := 200 ns;
        -- Warm HW reset
        tdevice_RPH              :VitalDelayType := 400 ns;
        --  Refresh time
        tdevice_REF100           :VitalDelayType :=  40 ns;
        --  Page Open Time
        tdevice_PO100            :VitalDelayType :=  40 ns;
        -- CSNeg Maximum Low Time
        tdevice_CSM              :VitalDelayType :=   4 us;

        -- generic control parameters
        InstancePath        :STRING  := DefaultInstancePath;
        TimingChecksOn      :BOOLEAN := DefaultTimingChecks;
        MsgOn               :BOOLEAN := DefaultMsgOn;
        XOn                 :BOOLEAN := DefaultXOn;

        -- memory file to be loaded
        mem_file_name       : STRING    := "s27kl0641.mem";
        UserPreload         : BOOLEAN   := FALSE;

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

    ATTRIBUTE VITAL_LEVEL0 of s27kl0641 : ENTITY IS TRUE;
END s27kl0641;

--------------------------------------------------------------------------------
-- ARCHITECTURE DECLARATION
--------------------------------------------------------------------------------
ARCHITECTURE vhdl_behavioral OF s27kl0641 IS
    ATTRIBUTE VITAL_LEVEL0 OF vhdl_behavioral : ARCHITECTURE IS TRUE;

    CONSTANT PartID         :STRING  := "s27kl0641";
    CONSTANT MaxData        :NATURAL := 16#FF#;
    CONSTANT MemSize        :NATURAL := 16#7FFFFF#; -- Bytes

    CONSTANT HiAddrBit      :NATURAL := 34;
    CONSTANT AddrRANGE      :NATURAL := 16#7FFFFF#; -- Bytes

    -- interconnect path delay signals
    SIGNAL DQ7_ipd          : std_ulogic := 'U';
    SIGNAL DQ6_ipd          : std_ulogic := 'U';
    SIGNAL DQ5_ipd          : std_ulogic := 'U';
    SIGNAL DQ4_ipd          : std_ulogic := 'U';
    SIGNAL DQ3_ipd          : std_ulogic := 'U';
    SIGNAL DQ2_ipd          : std_ulogic := 'U';
    SIGNAL DQ1_ipd          : std_ulogic := 'U';
    SIGNAL DQ0_ipd          : std_ulogic := 'U';

    SIGNAL CSNeg_ipd        : std_ulogic := 'U';
    SIGNAL CK_ipd           : std_ulogic := 'U';
    SIGNAL RESETNeg_ipd     : std_ulogic := 'U';
    SIGNAL RWDS_ipd         : std_ulogic := 'U';

    -- internal delays
    SIGNAL VCS_in           : std_ulogic := '0';
    SIGNAL VCS_out          : std_ulogic := '0';
    SIGNAL DPD_in           : std_ulogic := '0';
    SIGNAL DPD_out          : std_ulogic := '0';
    SIGNAL DPD_in_dly       : std_ulogic := '0';
    SIGNAL RPH_in           : std_ulogic := '0';
    SIGNAL RPH_out          : std_ulogic := '0';
    SIGNAL REF100_in        : std_ulogic := '0';
    SIGNAL REF100_out       : std_ulogic := '0';
    SIGNAL PO100_in         : std_ulogic := '0';
    SIGNAL PO100_out        : std_ulogic := '0';
    SIGNAL CSM_in           : std_ulogic := '0';
    SIGNAL CSM_out          : std_ulogic := '0';

    SIGNAL REF_in           : std_ulogic := '0';
    SIGNAL REF_out          : std_ulogic := '0';
    SIGNAL PO_in            : std_ulogic := '0';
    SIGNAL PO_out           : std_ulogic := '0';

    SIGNAL DPDExt_in        : std_ulogic := 'U';
    SIGNAL DPDExt_out       : std_ulogic := '0';
    SIGNAL DPDExt           : std_ulogic := '0';

BEGIN

    ---------------------------------------------------------------------------
    -- Internal Delays
    ---------------------------------------------------------------------------
    VCS      :VitalBuf(VCS_out  , VCS_in  , (tdevice_VCS     ,UnitDelay));
    DPD      :VitalBuf(DPDExt_out,DPDExt_in,(tdevice_DPDCSL  ,UnitDelay));
    RPH      :VitalBuf(RPH_out  , RPH_in  , (tdevice_RPH     ,UnitDelay));
    REF100   :VitalBuf(REF100_out,REF100_in,(tdevice_REF100  ,UnitDelay));
    PO100    :VitalBuf(PO100_out, PO100_in ,(tdevice_PO100   ,UnitDelay));
    CSM      :VitalBuf(CSM_out,   CSM_in   ,(tdevice_CSM     ,UnitDelay));
    ---------------------------------------------------------------------------
    -- Wire Delays
    ---------------------------------------------------------------------------
    WireDelay : BLOCK
    BEGIN

        w_1  : VitalWireDelay (DQ7_ipd, DQ7, tipd_DQ7);
        w_2  : VitalWireDelay (DQ6_ipd, DQ6, tipd_DQ6);
        w_3  : VitalWireDelay (DQ5_ipd, DQ5, tipd_DQ5);
        w_4  : VitalWireDelay (DQ4_ipd, DQ4, tipd_DQ4);
        w_5  : VitalWireDelay (DQ3_ipd, DQ3, tipd_DQ3);
        w_6  : VitalWireDelay (DQ2_ipd, DQ2, tipd_DQ2);
        w_7  : VitalWireDelay (DQ1_ipd, DQ1, tipd_DQ1);
        w_8  : VitalWireDelay (DQ0_ipd, DQ0, tipd_DQ0);

        w_9  : VitalWireDelay (CSNeg_ipd   , CSNeg   , tipd_CSNeg   );
        w_10 : VitalWireDelay (CK_ipd      , CK      , tipd_CK      );
        w_11 : VitalWireDelay (RESETNeg_ipd, RESETNeg, tipd_RESETNeg);
        w_12 : VitalWireDelay (RWDS_ipd    , RWDS    , tipd_RWDS   );

    END BLOCK WireDelay;

    ---------------------------------------------------------------------------
    -- Main Behavior Block
    ---------------------------------------------------------------------------
    Behavior : BLOCK
    PORT (
        DIn      :IN     std_logic_vector (7 DOWNTO 0) := (OTHERS => 'U');
        DOut     :OUT    std_ulogic_vector(7 DOWNTO 0) := (OTHERS => 'Z');

        CSNeg    :IN     std_ulogic := 'U';
        CK       :IN     std_ulogic := 'U';
        RESETNeg :IN     std_ulogic := 'U';
        RWDSIn   :IN     std_ulogic := 'U';
        RWDSOut  :OUT    std_ulogic := 'Z'
    );

    PORT MAP (
            DIn(7)      => DQ7_ipd,
            DIn(6)      => DQ6_ipd,
            DIn(5)      => DQ5_ipd,
            DIn(4)      => DQ4_ipd,
            DIn(3)      => DQ3_ipd,
            DIn(2)      => DQ2_ipd,
            DIn(1)      => DQ1_ipd,
            DIn(0)      => DQ0_ipd,

            DOut(7)     => DQ7,
            DOut(6)     => DQ6,
            DOut(5)     => DQ5,
            DOut(4)     => DQ4,
            DOut(3)     => DQ3,
            DOut(2)     => DQ2,
            DOut(1)     => DQ1,
            DOut(0)     => DQ0,

            CSNeg       => CSNeg_ipd,
            CK          => CK_ipd,
            RESETNeg    => RESETNeg_ipd,
            RWDSIn      => RWDS_ipd,
            RWDSOut     => RWDS
    );

    -- State Machine : State_Type
    TYPE state_type IS (
                        POWER_ON,
                        ACT,
                        RESET_STATE,
                        DPD_STATE);

    TYPE RD_MODE_type IS (LINEAR,
                          CONTINUOUS);

    TYPE bus_cycle_type IS (STAND_BY,
                            CA_BITS,
                            DATA_BITS);

    SHARED VARIABLE bus_cycle_state    : bus_cycle_type;

    TYPE MemArr    IS ARRAY (0 TO MemSize) OF INTEGER RANGE -1 TO MaxData;

    -- states
    SIGNAL current_state    : state_type;
    SIGNAL next_state       : state_type;

    SIGNAL RD_MODE          : RD_MODE_type;

    SHARED VARIABLE Config_reg : std_logic_vector(15 downto 0) := "1000111111111111";
    SIGNAL RW         : std_logic := '0';
    SHARED VARIABLE UByteMask  : std_logic := '0';
    SHARED VARIABLE LByteMask  : std_logic := '0';
    SIGNAL PoweredUp  : std_logic := '0';
    SIGNAL DPD_ACT    : std_logic := '0';

    --zero delay signals
    SIGNAL DOut_zd    : std_logic_vector(7 downto 0) := (OTHERS => 'Z');
    SIGNAL RWDS_zd    : std_logic := 'Z';

    SIGNAL CKDiff     : std_logic := 'Z';
    SIGNAL Target     : std_logic := '0';

    SHARED VARIABLE BurstDelay   : NATURAL RANGE 0 TO 6;
    SHARED VARIABLE RefreshDelay : NATURAL RANGE 0 TO 6;
    SHARED VARIABLE BurstLength  : NATURAL RANGE 0 TO 64;

    SIGNAL REFCOLL        : std_logic := '0';
    SIGNAL REFCOLL_ACTIV  : std_logic := '0';
    SIGNAL RWRCHECK       : std_logic := '0';

    SHARED VARIABLE CK_PER       : time      := 0 ns;
    -- timing check violation
    SIGNAL Viol                  : X01 := '0';

     -- Mem(Address)
    SHARED VARIABLE Mem     : MemArr   := (OTHERS => MaxData);

    SIGNAL RESETNeg_pullup  : std_logic := 'Z';

BEGIN

    current_state <= next_state;

    RESETNeg_pullup <= '1' WHEN RESETNeg = 'Z' ELSE
                       RESETNeg;

    PoweredUp <= '1' AFTER tdevice_VCS;

    REFIN: PROCESS(REF_in)
    BEGIN
        IF (TimingModel="S27KL0641DABHI000") OR
        (TimingModel="s27kl0641dabhi000") THEN
            REF100_in <= REF_in;
        END IF;
    END PROCESS;

    POIN: PROCESS(PO_in)
    BEGIN
        IF (TimingModel="S27KL0641DABHI000") OR
        (TimingModel="s27kl0641dabhi000") THEN
            PO100_in <= PO_in;
        END IF;
    END PROCESS;
    REF_out <= REF100_out;
    PO_out  <= PO100_out;

    CSMGen: PROCESS(CSNeg)
    BEGIN
        IF falling_edge(CSNeg) THEN
            CSM_in <= '1';
        ELSE
            CSM_in <= '0';
        END IF;
    END PROCESS;

    CSMCheck: PROCESS(CSNeg, CSM_out)
    BEGIN
        IF (CSNeg = '0' AND CSM_out = '1') THEN
            REPORT " tCSM time is violated. Please check the spec."
            SEVERITY WARNING;
        END IF;
    END PROCESS;

    ----------------------------------------------------------------------------
    -- CKDiff is not actualy diferential clock. CK# is used only for 1.8V
    ----------------------------------------------------------------------------
    CKDiff <= CK;

    ---------------------------------------------------------------------------
    -- VITAL Timing Checks Procedures
    ---------------------------------------------------------------------------
    VITALTimingCheck:PROCESS(DIn, RWDSIn, CSNeg, RESETNeg, CK)

        -- Timing Check Variables
        --Setup/Hold Check Variables
        VARIABLE Tviol_CSNeg_CK_R      : X01 := '0';
        VARIABLE TD_CSNeg_CK_R         : VitalTimingDataType;

        VARIABLE Tviol_CSNeg_CK_F      : X01 := '0';
        VARIABLE TD_CSNeg_CK_F         : VitalTimingDataType;

        VARIABLE Tviol_DQ0_CK_R        : X01 := '0';
        VARIABLE TD_DQ0_CK_R           : VitalTimingDataType;

        VARIABLE Tviol_DQ0_CK_F        : X01 := '0';
        VARIABLE TD_DQ0_CK_F           : VitalTimingDataType;

        VARIABLE Tviol_RWDS_CK_R       : X01 := '0';
        VARIABLE TD_RWDS_CK_R          : VitalTimingDataType;

        VARIABLE Tviol_RWDS_CK_F       : X01 := '0';
        VARIABLE TD_RWDS_CK_F          : VitalTimingDataType;

        VARIABLE Tviol_CSNeg_RESETNeg  : X01 := '0';
        VARIABLE TD_CSNeg_RESETNeg     : VitalTimingDataType;

        --Recovery Check Variable
        VARIABLE Rviol_CSNeg_CK_F      : X01 := '0';

        --Pulse Width and Period Check Variables
        VARIABLE Pviol_CK_p            : X01 := '0';
        VARIABLE PD_CK_p               : VitalPeriodDataType
                                                         := VitalPeriodDataInit;
        VARIABLE Pviol_CK              : X01 := '0';
        VARIABLE PD_CK                 : VitalPeriodDataType
                                                         := VitalPeriodDataInit;

        VARIABLE Pviol_CSNeg           : X01 := '0';
        VARIABLE PD_CSNeg              : VitalPeriodDataType
                                                         := VitalPeriodDataInit;
        VARIABLE Pviol_RESETNeg        : X01 := '0';
        VARIABLE PD_RESETNeg           : VitalPeriodDataType
                                                         := VitalPeriodDataInit;
        --Functionality Results Variables
        --(used to OR all individual violations)
        VARIABLE Violation        : X01 := '0';

    BEGIN

        ------------------------------------------------------------------------
        -- Timing Check Section
        ------------------------------------------------------------------------
        IF (TimingChecksOn) THEN

        -- Setup Check between CSNeg and CK
        VitalSetupHoldCheck (
            TestSignal      => CSNeg,
            TestSignalName  => "CS#",
            RefSignal       => CK,
            RefSignalName   => "CK",
            SetupLow        => tsetup_CSNeg_CK,
            SetupHigh       => tsetup_CSNeg_CK,
            CheckEnabled    => TRUE,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_CK_R,
            Violation       => Tviol_CSNeg_CK_R
        );

        -- Hold Check between CSNeg and CK
        VitalSetupHoldCheck (
            TestSignal      => CSNeg,
            TestSignalName  => "CS#",
            RefSignal       => CK,
            RefSignalName   => "CK",
            HoldLow         => thold_CSNeg_CK,
            CheckEnabled    => TRUE,
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_CK_F,
            Violation       => Tviol_CSNeg_CK_F
        );

        -- Setup Check/Hold between DATA and CK
        VitalSetupHoldCheck (
            TestSignal      => DIn,
            TestSignalName  => "DIn",
            RefSignal       => CK,
            RefSignalName   => "CK",
            SetupHigh       => tsetup_DQ0_CK,
            SetupLow        => tsetup_DQ0_CK,
            HoldHigh        => thold_DQ0_CK,
            HoldLow         => thold_DQ0_CK,
            CheckEnabled    => DOut_zd="ZZZZZZZZ",
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_DQ0_CK_R,
            Violation       => Tviol_DQ0_CK_R
        );

        -- Setup Check/Hold between DATA and CK
        VitalSetupHoldCheck (
            TestSignal      => DIn,
            TestSignalName  => "DIn",
            RefSignal       => CK,
            RefSignalName   => "CK",
            SetupHigh       => tsetup_DQ0_CK,
            SetupLow        => tsetup_DQ0_CK,
            HoldHigh        => thold_DQ0_CK,
            HoldLow         => thold_DQ0_CK,
            CheckEnabled    => DOut_zd="ZZZZZZZZ",
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_DQ0_CK_F,
            Violation       => Tviol_DQ0_CK_F
        );

        -- Setup Check/Hold between RWDS and CK during write operation
        VitalSetupHoldCheck (
            TestSignal      => RWDSIn,
            TestSignalName  => "RWDSIn",
            RefSignal       => CK,
            RefSignalName   => "CK",
            SetupHigh       => tsetup_DQ0_CK,
            SetupLow        => tsetup_DQ0_CK,
            HoldHigh        => thold_DQ0_CK,
            HoldLow         => thold_DQ0_CK,
            CheckEnabled    => RWDS_zd='Z' AND RW='0',
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_RWDS_CK_R,
            Violation       => Tviol_DQ0_CK_R
        );

        -- Setup Check/Hold between RWDS and CK during write operation
        VitalSetupHoldCheck (
            TestSignal      => RWDSIn,
            TestSignalName  => "RWDSIn",
            RefSignal       => CK,
            RefSignalName   => "CK",
            SetupHigh       => tsetup_DQ0_CK,
            SetupLow        => tsetup_DQ0_CK,
            HoldHigh        => thold_DQ0_CK,
            HoldLow         => thold_DQ0_CK,
            CheckEnabled    => RWDS_zd='Z' AND RW='0',
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_RWDS_CK_F,
            Violation       => Tviol_RWDS_CK_F
        );

        -- Hold Check between CS# and RESET#
        VitalSetupHoldCheck (
            TestSignal      => CSNeg,
            TestSignalName  => "CS#",
            RefSignal       => RESETNeg,
            RefSignalName   => "RESET#",
            HoldHigh        => thold_CSNeg_RESETNeg,
            CheckEnabled    => TRUE,
            RefTransition   => '/',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_RESETNeg,
            Violation       => Tviol_CSNeg_RESETNeg
        );

        -- Recovery Check between CSNeg and CK
        VitalRecoveryRemovalCheck (
            TestSignal      => CSNeg,
            TestSignalName  => "CS#",
            RefSignal       => CK,
            RefSignalName   => "CK",
            Recovery        => trecovery_CSNeg_CK_posedge_negedge,
            ActiveLow       => FALSE,
            CheckEnabled    => RWRCHECK = '1',
            RefTransition   => '\',
            HeaderMsg       => InstancePath & PartID,
            TimingData      => TD_CSNeg_CK_F,
            XOn             => XOn,
            MsgOn           => MsgOn,
            Violation       => Rviol_CSNeg_CK_F
        );

        -- Pulse Width Check for CK
        VitalPeriodPulseCheck (
            TestSignal      =>  CK,
            TestSignalName  =>  "CK",
            PulseWidthLow   =>  tpw_CK_negedge,
            PulseWidthHigh  =>  tpw_CK_posedge,
            PeriodData      =>  PD_CK_p,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_CK_p,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  TRUE);

        -- Pulse Width Check for CS#
        VitalPeriodPulseCheck (
            TestSignal      =>  CSNeg_ipd,
            TestSignalName  =>  "CS#",
            PulseWidthHigh  =>  tpw_CSNeg_posedge,
            PeriodData      =>  PD_CSNeg,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_CSNeg,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  TRUE);

        -- Pulse Width Check for RESETNeg
        VitalPeriodPulseCheck (
            TestSignal      =>  RESETNeg_ipd,
            TestSignalName  =>  "RESET#",
            PulseWidthLow   =>  tpw_RESETNeg_negedge,
            PeriodData      =>  PD_RESETNeg,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  Pviol_RESETNeg,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  TRUE);

        -- Period Check for CK
        VitalPeriodPulseCheck (
            TestSignal      =>  CK,
            TestSignalName  =>  "CK",
            Period          =>  tperiod_CK,
            PeriodData      =>  PD_CK,
            XOn             =>  XOn,
            MsgOn           =>  MsgOn,
            Violation       =>  PViol_CK,
            HeaderMsg       =>  InstancePath & PartID,
            CheckEnabled    =>  TRUE );

        Violation := Tviol_CSNeg_CK_R     OR
                     Tviol_CSNeg_CK_F     OR
                     Tviol_DQ0_CK_R       OR
                     Tviol_DQ0_CK_F       OR
                     Tviol_RWDS_CK_R      OR
                     Tviol_RWDS_CK_F      OR
                     Tviol_CSNeg_RESETNeg OR
                     Rviol_CSNeg_CK_F     OR
                     Pviol_CK_p           OR
                     Pviol_CK             OR
                     Pviol_CSNeg          OR
                     Pviol_RESETNeg;

            Viol <= Violation;

            ASSERT Violation = '0'
                REPORT InstancePath & partID & ": simulation may be" &
                        " inaccurate due to timing violations"
                SEVERITY WARNING;
        END IF;
    END PROCESS VITALTimingCheck;

    ---------------------------------------------------------------------------
    -- Bus Cycle Decode
    ----------------------------------------------------------------------------
    ReadWrite: PROCESS(CKDiff, CSNeg, DIn, REF_out, PO_out, RESETNeg)

        VARIABLE ca_cnt      : NATURAL RANGE 0 TO 48 := 48;
        VARIABLE data_cycle  : NATURAL := 0;
        VARIABLE ca_in       : std_logic_vector(47 downto 0);
        VARIABLE Data_in     : std_logic_vector(15 downto 0);
        VARIABLE Address     : NATURAL RANGE 0 TO AddrRANGE+1 := 0;
        VARIABLE Addr_bit    : std_logic_vector(22 downto 0);
        VARIABLE Start_BurstAddr : NATURAL RANGE 0 TO AddrRANGE := 0;
        VARIABLE RD_WRAP     : std_logic := '0';
        VARIABLE RdWrStart   : std_logic := '0';
        VARIABLE ALTERNATE_64: std_logic := '0';
        VARIABLE HYBRID      : std_logic := '0';

    BEGIN
    IF current_state = ACT THEN
        CASE bus_cycle_state IS
            WHEN STAND_BY =>
                IF falling_edge(CSNeg_ipd) THEN
                    ca_cnt    := 48;
                    data_cycle:= 0;
                    RD_WRAP   := '0';
                    RdWrStart := '0';
                    REFCOLL   <= '0';
                    REFCOLL_ACTIV <= '0';
                    ALTERNATE_64  := '0';
                    HYBRID    := '0';
                    bus_cycle_state := CA_BITS;
                END IF;

            WHEN CA_BITS =>

                IF (CSNeg = '0' AND CKDiff'EVENT) THEN
                    ca_in(ca_cnt-1 DOWNTO ca_cnt-8) := DIn(7 DOWNTO 0);
                    ca_cnt := ca_cnt - 8;
                    IF ca_cnt = 40 THEN
                        REFCOLL <= '1';
                        IF Config_reg(3) = '1' THEN -- fixed latency
                            REFCOLL_ACTIV <= '1';
                            RWDS_zd <= '1';
                        ELSIF Config_reg(3) = '0' THEN -- variable latency
                            RWDS_zd <= '0';
                        END IF;

                    ELSIF ca_cnt = 24 THEN
                        RWRCHECK <= '1';

                    ELSIF ca_cnt = 16 THEN
                        RW     <= ca_in(47);
                        Target <= ca_in(46);
                        IF ca_in(46) = '0' OR (ca_in(46) = '1' AND ca_in(47) = '1') THEN
                            IF REFCOLL_ACTIV = '1' THEN
                                REF_in <= '1';
                            ELSE
                                PO_in <= '1';
                            END IF;
                        END IF;
                        IF Config_reg(2) = '0' THEN
                            HYBRID := '1';
                        END IF;
                        IF Config_reg(1 DOWNTO 0) = "00" OR
                            Config_reg(1 DOWNTO 0) = "01" THEN
                            BurstLength := 64;
                            IF Config_reg(1 DOWNTO 0) = "01" THEN
                                ALTERNATE_64 := '1';
                            END IF;
                        ELSIF Config_reg(1 DOWNTO 0) = "10" THEN
                            BurstLength := 16;
                        ELSIF Config_reg(1 DOWNTO 0) = "11" THEN
                            BurstLength := 32;
                        END IF;
                        IF Config_reg(7 downto 4) = "0000"  THEN
                            BurstDelay := 5;
                        ELSIF Config_reg(7 downto 4) = "0001"  THEN
                            BurstDelay := 6;
                        ELSIF Config_reg(7 downto 4) = "1111"  THEN
                            BurstDelay := 4;
                        ELSIF Config_reg(7 downto 4) = "1110"  THEN
                            BurstDelay := 3;
                        END IF;

                        RefreshDelay := BurstDelay;

                    ELSIF ca_cnt = 8 THEN
                        IF RW = '1' THEN-- read
                            RWDS_zd <= '0';
                        ELSE  -- write
                            RWDS_zd <= 'Z';
                        END IF;
                        RWRCHECK <= '0';

                    ELSIF ca_cnt = 0 THEN
                        REFCOLL <= '0';
                        Address := 2* to_nat(ca_in(HiAddrBit DOWNTO 16) &
                                           ca_in(2 DOWNTO 0));

                        IF ca_in(45) = '1' THEN
                            RD_MODE <= CONTINUOUS;
                        ELSE
                            RD_MODE <= LINEAR;
                        END IF;

                        Start_BurstAddr := Address;
                        Addr_bit := to_slv(Address,23);

                        IF REFCOLL_ACTIV = '1' THEN
                            RefreshDelay := RefreshDelay - 1;
                        ELSE
                            BurstDelay := BurstDelay - 1;
                        END IF;

                        bus_cycle_state := DATA_BITS;
                    END IF;
                END IF;

            WHEN DATA_BITS =>

                IF rising_edge(CKDiff) AND CSNeg = '0' THEN
                    IF Target = '1' AND RW = '0' THEN
                        Data_in(15 DOWNTO 8) := DIn(7 DOWNTO 0);
                        data_cycle := data_cycle+1;
                    ELSE
                        IF (BurstDelay = 0 AND PO_in = '0') THEN
                            RdWrStart := '0';
                            IF RW = '1' THEN -- read
                                RWDS_zd <= '1';
                                IF Target = '0' THEN --mem array
                                    IF Mem(Address+1) = -1 THEN
                                        DOut_zd <= "XXXXXXXX"
                                                        AFTER tpd_CK_DQ0(tr01);
                                    ELSE
                                        DOut_zd <= to_slv(Mem(Address+1), 8);
                                    END IF;
                                ELSE -- reg
                                    DOut_zd <= Config_reg(15 DOWNTO 8);
                                END IF;
                            ELSE -- write
                                data_cycle := data_cycle + 1;
                                Data_in(15 DOWNTO 8) := DIn(7 DOWNTO 0);
                                UByteMask := RWDSIn;
                            END IF;
                        END IF;
                    END IF;
                END IF;

                IF falling_edge(CKDiff) AND CSNeg = '0' THEN
                    IF Target = '1' AND RW = '0' THEN
                        Data_in(7 DOWNTO 0) := DIn(7 DOWNTO 0);
                        data_cycle := data_cycle+1;
                        IF data_cycle = 2 THEN
                            IF Data_in(15) = '0' AND Config_reg(15) = '1' THEN
                                DPD_ACT <= '1', '0' AFTER 1 ns;
                            END IF;
                            Config_reg := Data_in;
                        END IF;
                    ELSE
                        IF REFCOLL_ACTIV = '1' THEN
                            IF RefreshDelay > 0 THEN
                                RefreshDelay := RefreshDelay-1;
                            END IF;
                            IF RefreshDelay = 0 THEN
                                IF REF_in = '0' THEN
                                    PO_in <= '1';
                                    REFCOLL_ACTIV <= '0';
                                END IF;
                            END IF;
                        ELSE
                            IF BurstDelay>0 THEN
                                BurstDelay := BurstDelay - 1;
                            ELSE
                                IF PO_in = '0' THEN
                                    IF RdWrStart = '1' THEN
                                        RdWrStart := '0';
                                    ELSE
                                        IF RW = '1' THEN -- read
                                            RWDS_zd <= '0';
                                            IF Target = '0' THEN
                                                IF Mem(Address) = -1 THEN
                                                    DOut_zd <= "XXXXXXXX"
                                                    AFTER tpd_CK_DQ0(tr01);
                                                ELSE
                                                    DOut_zd <=
                                                    to_slv(Mem(Address), 8);
                                                END IF;
                                            ELSE
                                                DOut_zd <= Config_reg(7 DOWNTO 0);
                                            END IF;
                                        ELSE -- write
                                            IF Target = '0' THEN
                                                IF data_cycle >= 1 THEN
                                                    Data_in(7 DOWNTO 0) := DIn(7 DOWNTO 0);
                                                    data_cycle := data_cycle + 1;
                                                    LByteMask := RWDS;
                                                    IF data_cycle MOD 2 = 0 THEN
                                                        IF LByteMask = '0' THEN
                                                            Mem(Address) :=
                                                            to_nat(Data_in(7 DOWNTO 0));
                                                        END IF;
                                                        IF UByteMask = '0' THEN
                                                            Mem(Address+1) :=
                                                            to_nat(Data_in(15 DOWNTO 8));
                                                        END IF;
                                                    END IF;
                                                END IF;
                                            END IF;
                                        END IF;

                                        IF RD_MODE = CONTINUOUS THEN
                                            IF Address = AddrRANGE - 1 THEN
                                                Address := 0;
                                            ELSE
                                                Address := Address + 2;
                                            END IF;
                                        ELSE -- wrapped burst
                                            IF HYBRID = '0' THEN --legacy wrapped burst
                                                IF (BurstLength = 16) OR
                                                (BurstLength = 32) OR
                                                (BurstLength = 64 AND
                                                ALTERNATE_64 = '0') THEN

                                                    Address := Address + 2;

                                                    IF Address MOD
                                                    BurstLength = 0 THEN
                                                        Address :=
                                                    Address - BurstLength;
                                                    END IF;
                                                ELSIF BurstLength = 64
                                                AND ALTERNATE_64 = '1' THEN
                                                    Address := Address + 2;
                                                    Addr_bit :=to_slv(Address,23);

                                                    IF Address MOD
                                                    (BurstLength/2) = 0 THEN

                                                        IF RD_WRAP='0' THEN
                                                            Address:=
                                                Address-(BurstLength/2);
                                                            RD_WRAP := '1';
                                                        ELSE
                                                            ALTERNATE_64:='0';
                                                            IF Addr_bit(5)='0' THEN
                                                                Address:=
                                                            Address-BurstLength;
                                                            END IF;
                                                        END IF;
                                                    END IF;
                                                    IF Address =
                                                    Start_BurstAddr AND
                                                    ALTERNATE_64='1' THEN
                                                        Addr_bit :=to_slv(Address,23);
                                                        IF Addr_bit(5)='0' THEN
                                                            Address:=
                                                        (Start_BurstAddr/
                                                        BurstLength)*BurstLength
                                                        + (BurstLength/2);

                                                        ELSIF Addr_bit(5)='1' THEN
                                                            Address:=
                                                        (Start_BurstAddr/
                                                        BurstLength)*BurstLength;
                                                        END IF;
                                                    END IF;
                                                END IF;

                                            ELSE -- Hybrid

                                                IF (BurstLength=16) OR
                                                (BurstLength=32) OR
                                                (BurstLength=64 AND
                                                ALTERNATE_64='0') THEN

                                                    Address := Address + 2;
                                                    Addr_bit :=to_slv(Address,23);

                                                    IF Address MOD
                                                    BurstLength = 0 THEN
                                                        Address :=
                                                    Address - BurstLength;
                                                    END IF;

                                                    IF Address =
                                                    Start_BurstAddr THEN
                                                        Address:=
                                                        (Start_BurstAddr/
                                                        BurstLength)*BurstLength
                                                        + BurstLength;

                                                        IF Address = AddrRANGE+1 THEN
                                                            Address := 0;
                                                        END IF;
                                                        RD_MODE <= CONTINUOUS;
                                                    END IF;

                                                ELSIF BurstLength=64
                                                AND ALTERNATE_64='1' THEN

                                                    Address := Address + 2;
                                                    Addr_bit :=to_slv(Address,23);

                                                    IF Address MOD
                                                    (BurstLength/2) = 0 THEN

                                                        IF RD_WRAP='0' THEN
                                                            Address:=
                                                Address-(BurstLength/2);
                                                            RD_WRAP := '1';
                                                        ELSE
                                                            Address:=
                                                        (Start_BurstAddr/
                                                        BurstLength)*BurstLength
                                                        + BurstLength;
                                                            IF Address=AddrRANGE+1 THEN
                                                                Address := 0;
                                                            END IF;
                                                            RD_MODE <=
                                                            CONTINUOUS;
                                                        END IF;
                                                    END IF;

                                                    IF Address =
                                                    Start_BurstAddr THEN
                                                        Addr_bit :=to_slv(Address,23);
                                                        IF Addr_bit(5)='0' THEN
                                                            Address:=
                                                        (Start_BurstAddr/
                                                        BurstLength)*BurstLength
                                                        + (BurstLength/2);

                                                        ELSIF Addr_bit(5)='1' THEN
                                                            Address:=
                                                        (Start_BurstAddr/
                                                        BurstLength)*BurstLength;
                                                        END IF;
                                                    END IF;
                                                END IF;
                                            END IF;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                END IF;

           WHEN OTHERS => NULL;
        END CASE;
        IF falling_edge(CSNeg) THEN
            IF Config_reg(3) = '1' THEN -- fixed latency
                RWDS_zd <= '1';
            ELSE -- variable latency
                RWDS_zd <= '0';
            END IF;
        END IF;
        IF rising_edge(CSNeg) OR falling_edge(RESETNeg) THEN
            bus_cycle_state := STAND_BY;
            DOut_zd <= "ZZZZZZZZ";
            RWDS_zd <= 'Z';
            IF falling_edge(RESETNeg) THEN
                Config_reg := "1000111100011111";-- default value
            END IF;
        END IF;
        IF rising_edge(PO_out) THEN
            PO_in <= '0';
            RdWrStart := '1';
        END IF;

        IF rising_edge(REF_out) THEN
            REF_in <= '0';
        END IF;
    ELSE
        bus_cycle_state := STAND_BY;
    END IF;
    END PROCESS ReadWrite;

    ---------------------------------------------------------------------------
    -- DPD timing control
    ---------------------------------------------------------------------------
    -- DPDExit_in is any write or read access for which CSNeg_ipd is asserted
    -- more than tDPDCSL time
    DPDExt_in <= '1' WHEN (falling_edge(CSNeg_ipd) AND (DPD_in = '1')) ELSE
                 '0';
    -- If Hardware Reset is an event to exit DPD mode generate new signal which
    -- purpose is to delay exiting from DPD state to IDLE after tDPD time
    DPD_in_dly <= DPD_in after 1 ns;

    DPDExit : PROCESS (CSNeg_ipd, DPDExt_out, RESETNeg, DPD_in_dly)
    BEGIN
        IF ((rising_edge(DPDExt_out) AND (CSNeg_ipd = '0')) OR
            (falling_edge(RESETNeg) AND (DPD_in_dly = '1')))THEN
            DPDExt <= '1', '0' AFTER 1 ns;
        END IF;
    END PROCESS;

    DPDTime : PROCESS (DPDExt)
    BEGIN
        IF rising_edge(DPDExt) THEN
            DPD_out <= '0', '1' AFTER tdevice_DPD;
        END IF;
    END PROCESS DPDTime;

    StateGen :PROCESS (PoweredUp, RPH_out, DPD_in, DPD_out, RESETNeg)

    BEGIN
        CASE current_state IS
            WHEN POWER_ON =>
                IF (rising_edge(PoweredUp)) THEN
                    next_state <= ACT;
                END IF;

            WHEN ACT =>
                IF (falling_edge(RESETNeg)) THEN
                    next_state <= RESET_STATE;
                ELSIF rising_edge(DPD_in) THEN
                    next_state <= DPD_STATE;
                END IF;

            WHEN RESET_STATE =>
                IF (rising_edge(RPH_out) AND RESETNeg_pullup = '1') OR
                (rising_edge(RESETNeg) AND RPH_in = '0') THEN
                    next_state <= ACT;
                END IF;

            WHEN DPD_STATE =>
                IF (falling_edge(RESETNeg)) THEN
                    next_state <= RESET_STATE;
                ELSIF (rising_edge(DPD_out)) THEN
                    next_state <= ACT;
                END IF;

            WHEN OTHERS => null;

        END CASE;

    END PROCESS StateGen;

Functional:PROCESS(DPD_ACT, DPD_out, RPH_out, RESETNeg)

    BEGIN
    CASE current_state IS

        WHEN POWER_ON =>

        WHEN ACT =>
            IF falling_edge(RESETNeg) THEN
                RPH_in <= '1';
            END IF;

            IF rising_edge(DPD_ACT) THEN
                DPD_in <= '1';
            END IF;

        WHEN RESET_STATE =>
            IF (rising_edge(RPH_out)) THEN
                RPH_in <= '0';
            END IF;

        WHEN DPD_STATE =>
            IF (rising_edge(DPD_out)) THEN
                DPD_in <= '0';
            END IF;

            IF falling_edge(RESETNeg) THEN
                RPH_in <= '1';
                DPD_in <= '0';
            END IF;

        WHEN OTHERS => null;

        END CASE;

    END PROCESS Functional;

PRELOAD : PROCESS
        -- text file input variables
        FILE mem_file          : text  is  mem_file_name;
        VARIABLE buf           : line;
        VARIABLE addr_ind      : NATURAL;
        VARIABLE ind           : NATURAL := 0;
        VARIABLE mem_data      : std_logic_vector(15 downto 0);
    BEGIN
        ---- File Read Section - Preload Control
        IF NOW = 0 ns  THEN

        ------------------------------------------------------------------------
        -----s27kl0641 memory preload file format -----------------------------
        ------------------------------------------------------------------------
        --   /          - comment
        --   @aaaaaaaa  - <aaaaaaa> stands for address within sector
        --   dddd       - <dddd> is word to be written at Mem(*)(aaaaaaaa++)
        --                (aaaaaaaa is incremented at every load)
        --   only first 1-7 columns are loaded. NO empty lines !!!!!!!!!!!!!!!!
        ------------------------------------------------------------------------
            IF (mem_file_name(1 to 4) /= "none" ) THEN
                addr_ind := 0;
                Mem := (OTHERS => MaxData);
                WHILE (not ENDFILE (mem_file)) LOOP
                    READLINE (mem_file, buf);
                    IF buf(1) = '/' THEN --comment
                        NEXT;
                    ELSIF buf(1) = '@' THEN --address
                        addr_ind := h(buf(2 to 7));
                    ELSE
                        IF addr_ind < (MemSize + 1)/2 THEN
                            mem_data := to_slv(h(buf(1 to 4)), 16);
                            Mem(2*addr_ind) := to_nat(mem_data(7 DOWNTO 0));
                            Mem(2*addr_ind+1):= to_nat(mem_data(15 DOWNTO 8));
                            addr_ind := addr_ind + 1;
                        END IF;
                    END IF;
                END LOOP;
            END IF;
        END IF;

        WAIT;
    END PROCESS PRELOAD;

    ----------------------------------------------------------------------------
    -- Path Delay Section
    ----------------------------------------------------------------------------
    PROCESS(RWDS_zd)
            VARIABLE RWDS_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z(
                OutSignal           => RWDSOut,
                OutSignalName       => "RWDSOut",
                OutTemp             => RWDS_zd,
                GlitchData          => RWDS_GlitchData,
                Mode                => VitalTransport,
                Paths               => (
                0 => (InputChangeTime => CSNeg'LAST_EVENT,
                      PathDelay       => tpd_CSNeg_RWDS,
                      PathCondition   => TRUE),

                1 => (InputChangeTime => CKDiff'LAST_EVENT,
                      PathDelay       => tpd_CK_RWDS,
                      PathCondition   => TRUE)
                )
            );
        END PROCESS;

    ---------------------------------------------------------------------------
    -- Path Delay Section for DOut signal
    ---------------------------------------------------------------------------
    D_Out_PathDelay_Gen : FOR i IN 0 TO 7 GENERATE
        PROCESS(DOut_zd(i))
            VARIABLE D0_GlitchData : VitalGlitchDataType;
        BEGIN
            VitalPathDelay01Z(
                OutSignal           => DOut(i),
                OutSignalName       => "DOut",
                OutTemp             => DOut_zd(i),
                GlitchData          => D0_GlitchData,
                Mode                => VitalTransport,
                Paths               => (

                0 => (InputChangeTime => CSNeg'LAST_EVENT,
                      PathDelay       => tpd_CSNeg_DQ0,
                      PathCondition   => TRUE),

                1 => (InputChangeTime => CKDiff'LAST_EVENT,
                      PathDelay       => tpd_CK_DQ0,
                      PathCondition   => TRUE)
                )
            );
        END PROCESS;
    END GENERATE D_Out_PathDelay_Gen;

    END BLOCK Behavior;
END vhdl_behavioral;