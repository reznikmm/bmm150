--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

pragma Ada_2022;

with BMM150.Raw;

package body BMM150.Internal is

   subtype Byte is Interfaces.Unsigned_8;
   use type Byte;

   -------------------
   -- Check_Chip_Id --
   -------------------

   function Check_Chip_Id
     (Device : Device_Context;
      Expect : Interfaces.Unsigned_8) return Boolean
   is
      Ok   : Boolean;
      Data : BMM150.Raw.Chip_Id_Data;
   begin
      Read (Device, Data, Ok);

      return Ok and BMM150.Raw.Get_Chip_Id (Data) = Expect;
   end Check_Chip_Id;

   ---------------------------
   -- Set_Interrupt_Setting --
   ---------------------------

   procedure Set_Interrupt_Setting
     (Device          : Device_Context;
      Interrupt_Pin   : Boolean := False;
      Data_Ready_Pin  : Boolean := False;
      Data_Ready_High : Boolean := True;
      Interrupt_High  : Boolean := True;
      Interrupt_Latch : Boolean := True;
      Success         : out Boolean)
   is
      Data : constant BMM150.Raw.Interrupt_Setting_Data :=
        BMM150.Raw.Set_Interrupt_Setting
          (Interrupt_Pin   => Interrupt_Pin,
           Data_Ready_Pin  => Data_Ready_Pin,
           Data_Ready_High => Data_Ready_High,
           Interrupt_High  => Interrupt_High,
           Interrupt_Latch => Interrupt_Latch);
   begin
      Write (Device, Data'First, Data (Data'First), Success);
   end Set_Interrupt_Setting;

   ---------------------
   -- Set_Repetitions --
   ---------------------

   procedure Set_Repetitions
     (Device  : Device_Context;
      Preset  : Setting;
      Success : out Boolean)
   is
      XY_Data : constant BMM150.Raw.XY_Repetitions_Data :=
        BMM150.Raw.Set_XY_Repetitions (Preset);
      Z_Data : constant BMM150.Raw.Z_Repetitions_Data :=
        BMM150.Raw.Set_Z_Repetitions (Preset);
   begin
      Write (Device, XY_Data'First, XY_Data (XY_Data'First), Success);

      if Success then
         Write (Device, Z_Data'Last, Z_Data (Z_Data'Last), Success);
      end if;
   end Set_Repetitions;

   -----------------
   -- Set_Suspend --
   -----------------

   procedure Set_Suspend
     (Device     : Device_Context;
      Suspend    : Boolean;
      SPI_3_Wire : Boolean := False;
      Success    : out Boolean)
   is
      Data : constant BMM150.Raw.Suspend_Data :=
        BMM150.Raw.Set_Suspend (Suspend, SPI_3_Wire);
   begin
      Write (Device, Data'First, Data (Data'First), Success);
   end Set_Suspend;

   -------------------------
   -- Read_Trim_Registers --
   -------------------------

   procedure Read_Trim_Registers
     (Device  : Device_Context;
      Value   : out Trim_Registers;
      Success : out Boolean)
   is
      Data : BMM150.Raw.Trim_Registers_Data;
   begin
      Read (Device, Data, Success);

      if Success then
         Value := BMM150.Raw.Get_Trim_Registers (Data);
      end if;
   end Read_Trim_Registers;

   ---------------
   -- Measuring --
   ---------------

   function Measuring (Device : Device_Context) return Boolean is
      Raw : Byte_Array (16#48# .. 16#48#);
      Ok  : Boolean;
   begin
      Read (Device, Raw, Ok);

      return Ok and then BMM150.Raw.Is_Measuring (Raw);
   end Measuring;

   ----------------------
   -- Read_Measurement --
   ----------------------

   procedure Read_Measurement
     (Device  : Device_Context;
      Trim    : Trim_Registers;
      Value   : out Optional_Magnetic_Field_Vector;
      Success : out Boolean)
   is
      Data : BMM150.Raw.Measurement_Data;
   begin
      Read (Device, Data, Success);

      if Success and then BMM150.Raw.Is_Valid (Data) then
         Value := BMM150.Raw.Get_Measurement (Data, Trim);
      else
         Value := (X | Y | Z => 0.0, Overflow => True);
         Success := False;
      end if;
   end Read_Measurement;

   --------------------------
   -- Read_Raw_Measurement --
   --------------------------

   procedure Read_Raw_Measurement
     (Device  : Device_Context;
      Value   : out Raw_Density_Vector;
      R_Hall  : out BMM150.Raw_Hall;
      Success : out Boolean)
   is
      Data : BMM150.Raw.Measurement_Data;
   begin
      Read (Device, Data, Success);

      if Success then
         BMM150.Raw.Get_Raw_Measurement (Data, Value, R_Hall);
      else
         Value := (X | Y | Z => 0);
         R_Hall := 0;
      end if;
   end Read_Raw_Measurement;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Device  : Device_Context;
      Success : out Boolean)
   is
      Data : BMM150.Raw.Suspend_Data := BMM150.Raw.Reset;
   begin
      Write (Device, Data'First, Data (Data'First), Success);

      if not Success then
         return;
      end if;

      for J in 1 .. 300 loop
         Read (Device, Data, Success);

         if Success and then not BMM150.Raw.Is_Reseting (Data) then
            return;
         end if;
      end loop;

      Success := False;
   end Reset;

   --------------------
   -- Set_Power_Mode --
   --------------------

   procedure Set_Power_Mode
     (Device  : Device_Context;
      Mode    : Power_Mode;
      ODR     : Output_Data_Rate;
      Success : out Boolean)
   is
      Data : constant BMM150.Raw.Power_Mode_Data :=
        BMM150.Raw.Set_Power_Mode (Mode, ODR);
   begin
      Write (Device, Data'First, Data (Data'First), Success);
   end Set_Power_Mode;

end BMM150.Internal;
