--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

pragma Ada_2022;

with Ada.Unchecked_Conversion;

package body BMM150.Internal is

   subtype Byte is Interfaces.Unsigned_8;

   Small_14 : constant := 1.0 / 2.0 ** 14;
   type Fixed_14 is delta Small_14 range -2.0 ** 17 .. 2.0 ** 17 - Small_14;
   subtype Fixed_14_Unit is Fixed_14 range -1.0 + Small_14 .. 1.0 - Small_14;

   procedure Compensate_XY
     (X      : Raw_XY;
      A0     : Fixed_14_Unit;
      Reg    : Trim_Registers;
      X_1    : Interfaces.Integer_8;
      X_2    : Interfaces.Integer_8;
      Result : out Density)
        with SPARK_Mode, Inline;
   --  Part of compensation procedure for X and Y

   procedure Compensate_Z
     (Z      : Raw_Z;
      Hall   : Raw_Hall;
      Reg    : Trim_Registers;
      Result : out Density)
        with SPARK_Mode, Inline;
   --  Part of compensation procedure for Z

   -------------------
   -- Check_Chip_Id --
   -------------------

   function Check_Chip_Id
     (Device : Device_Context;
      Expect : Interfaces.Unsigned_8) return Boolean
   is
      Ok   : Boolean;
      Data : Byte_Array (16#40# .. 16#40#);
   begin
      Read (Device, Data, Ok);

      return Ok and Data = [Expect];
   end Check_Chip_Id;

   ---------------------
   -- Set_Repetitions --
   ---------------------

   procedure Set_Repetitions
     (Device  : Device_Context;
      Preset  : Setting;
      Success : out Boolean) is
   begin
      Write (Device, 16#51#, Byte (Preset.X_Y / 2), Success);

      if Success then
         Write (Device, 16#52#, Byte (Preset.Z / 2), Success);
      end if;
   end Set_Repetitions;

   -----------------
   -- Set_Suspend --
   -----------------

   procedure Set_Suspend
     (Device     : Device_Context;
      Timer      : not null HAL.Time.Any_Delays;
      Suspend    : Boolean;
      SPI_3_Wire : Boolean := False;
      Success    : out Boolean)
   is
      Raw : constant Byte :=
        (if Suspend then 0 elsif SPI_3_Wire then 4 else 1);
   begin
      Write (Device, 16#4B#, Raw, Success);

      if not Suspend then
         Timer.Delay_Milliseconds (3);
      end if;
   end Set_Suspend;

   -------------------------
   -- Read_Trim_Registers --
   -------------------------

   procedure Read_Trim_Registers
     (Device  : Device_Context;
      Value   : out Trim_Registers;
      Success : out Boolean)
   is
      use Interfaces;

      function Cast_8 is new Ada.Unchecked_Conversion (Byte, Integer_8);

      function Cast is new Ada.Unchecked_Conversion
        (Unsigned_16, Integer_16);

      function To_Unsigned (LSB, MSB : Byte) return Unsigned_16 is
        (Unsigned_16 (LSB) + Shift_Left (Unsigned_16 (MSB), 8));

      function To_Integer (LSB, MSB : Byte) return Integer_16 is
        (Cast (To_Unsigned (LSB, MSB)));

      Z2 : Integer_16;

   begin
      declare
         X1Y1 : Byte_Array (16#5D# .. 16#5E#);
      begin
         Read (Device, X1Y1, Success);

         if Success then
            Value.X1 := Cast_8 (X1Y1 (X1Y1'First));
            Value.Y1 := Cast_8 (X1Y1 (X1Y1'First + 1));
         else
            return;
         end if;
      end;

      declare
         XYZ  : Byte_Array (16#62# .. 16#65#);
      begin
         Read (Device, XYZ, Success);

         if Success then
            Value.Z4 := To_Integer (XYZ (XYZ'First), XYZ (XYZ'First + 1));
            Value.X2 := Cast_8 (XYZ (XYZ'First + 2));
            Value.Y2 := Cast_8 (XYZ (XYZ'First + 3));
         else
            return;
         end if;
      end;

      declare
         XY12 : Byte_Array (16#68# .. 16#71#);
      begin
         Read (Device, XY12, Success);

         if not Success then
            return;
         end if;

         Value.Z1 :=
           To_Unsigned (XY12 (XY12'First + 2), XY12 (XY12'First + 3));

         Z2 := To_Integer (XY12 (XY12'First), XY12 (XY12'First + 1));
         Success := Z2 > 0;
         Value.Z2 := (if Z2 > 0 then Z2 else 1);
         Value.Z3 := To_Integer (XY12 (XY12'First + 6), XY12 (XY12'First + 7));

         Value.XY1 := XY12 (XY12'First + 9);
         Value.XY2 := Cast_8 (XY12 (XY12'First + 8));

         Value.XYZ1 := To_Unsigned
           (XY12 (XY12'First + 4),
            XY12 (XY12'First + 5) and 127);
      end;
   end Read_Trim_Registers;

   ----------------
   -- Compensate --
   ----------------

   procedure Compensate_XY
     (X      : Raw_XY;
      A0     : Fixed_14_Unit;
      Reg    : Trim_Registers;
      X_1    : Interfaces.Integer_8;
      X_2    : Interfaces.Integer_8;
      Result : out Density)
   is
      pragma SPARK_Mode;
      pragma Suppress (All_Checks);

      --  These fixed type were chosen to match exact values of the formula
      --  in C file.

      Small_28 : constant := 1.0 / 2.0 ** 28;
      type Fixed_28 is delta Small_28 range -1.0 .. 1.0 - Small_28;

      Small_13 : constant := 1.0 / 2.0 ** 13;
      type Fixed_13 is delta Small_13 range 0.0 .. 18.0 - Small_13;

      Small_19 : constant := 1.0 / 2.0 ** 19;
      type Fixed_19 is delta Small_19 range 0.0 .. 4.0 - Small_19;

      Small_16 : constant := 1.0 / 2.0 ** 16;
      type Fixed_16 is delta Small_16 range -74_000.0 .. 74_000.0 - Small_16;

      Small_7 : constant := 1.0 / 2.0 ** 7;
      type Fixed_7 is delta Small_7 range -1.0 .. 1.0 - Small_7;

      Small_6 : constant := 1.0 / 2.0 ** 6;
      type Fixed_6 is delta Small_6 range 0.0 .. 4.5;

      Small_2 : constant := 1.0 / 2.0;
      type Fixed_2 is delta Small_2 range -64.0 .. 64.0 - Small_2;

      Field_Small : constant Fixed_16 := Density'Small;

      XY2 : constant Fixed_7 := Fixed_7'Small * Integer (Reg.XY2);
      XY1 : constant Fixed_7 := Fixed_7'Small * Natural (Reg.XY1);
      X2  : constant Fixed_6 := Fixed_6'Small * (Integer (X_2)) + 2.5;
      X1  : constant Fixed_2 := Fixed_2'Small * Integer (X_1);

      A3 : constant Fixed_28 := A0 * A0;   --  0 .. 1
      A4 : constant Fixed_28 := XY2 * A3;  --  -1 .. 1
      A6 : constant Fixed_28 := A0 * XY1;  --  -1 .. 1
      A7 : constant Fixed_19 := Fixed_19'Base (A4 + A6) + 2.0;  --  0 .. 4
      A9 : constant Fixed_13 := A7 * X2;  -- 0 .. 9
      A1 : constant Fixed_16 := A9 * Integer (X) * Field_Small;  --  (-9..9)*x
      A8 : constant Fixed_16 := A1 + Fixed_16 (X1);

   begin
      if A8 in Fixed_16 (Density'First) .. Fixed_16 (Density'Last) then
         Result := Density (A8);
      else
         Result := Density'First;
      end if;
   end Compensate_XY;

   ------------------
   -- Compensate_Z --
   ------------------

   procedure Compensate_Z
     (Z      : Raw_Z;
      Hall   : Raw_Hall;
      Reg    : Trim_Registers;
      Result : out Density)
   is
      type Wide_Fixed_4 is delta Small_4
        range -2.0 ** 27 .. 2.0 ** 27 - Small_4;

      Small_6 : constant := 1.0 / 2.0 ** 6;
      type Fixed_6 is delta Small_6 range -2.0**9 .. 2.0**9 - Small_6;

      Small_15 : constant := 1.0 / 2.0 ** 15;
      type Fixed_15 is delta Small_15 range -1.0 .. 1.0 - Small_15;

      type Wide_Fixed_15 is delta Small_15
        range -2.0 ** 16 .. 2.0 ** 16 - Small_15;

      Z_1 : constant Fixed_15 range Small_15 .. 1.0 - Small_15 :=
        Fixed_15'Small * Positive (Reg.Z1);

      Z_2 : constant Fixed_15 range Small_15 .. 1.0 - Small_15 :=
        Fixed_15'Small * Integer (Reg.Z2);

      Z_3 : constant Fixed_6 range -512.0 .. 512.0 - Small_6 :=
        Fixed_6'Small * Integer (Reg.Z3);

      Z_4 : constant Wide_Fixed_4 range -2048.0 .. 2048.0 - Small_4 :=
        Wide_Fixed_4'Small * Integer (Reg.Z4);

      ZZ : constant Wide_Fixed_4 range -1024.0 .. 1024.0 - Small_4 :=
        Wide_Fixed_4'Small * Integer (Z);

      H   : constant Fixed_15 range Small_15 .. 0.5 - Small_15 :=
        Fixed_15'Small * Integer (Hall);

      Hall_XYZ : constant Integer range -2**15 + 2 .. 2**14 - 2 :=
        Positive (Hall) - Positive (Reg.XYZ1);

      Diff : constant Fixed_15 range -1.0 .. 0.5 := Fixed_15'Small * Hall_XYZ;

      A1  : constant Fixed_15 range 0.0 .. 0.5 - Small_15 := Z_1 * H;
      A3  : constant Wide_Fixed_15 range Small_15 .. 1.5 - Small_15  :=
        Wide_Fixed_15 (Z_2) + Wide_Fixed_15 (A1);

      A4  : constant Wide_Fixed_15 range -512.0 .. 512.0 := Z_3 * Diff;

      A5  : constant Wide_Fixed_4 := (Wide_Fixed_15 (ZZ - Z_4) - A4) / A3;
   begin
      if A5 in Wide_Fixed_4 (Density'First) .. Wide_Fixed_4 (Density'Last) then
         Result := Density (A5);
      else
         Result := Density'First;
      end if;
   end Compensate_Z;

   ---------------
   -- Measuring --
   ---------------

   function Measuring (Device : Device_Context) return Boolean is
      use type Byte;

      Raw : Byte_Array (16#48# .. 16#48#);
      Ok  : Boolean;
   begin
      Read (Device, Raw, Ok);

      return Ok and then (Raw (Raw'First) and 1) = 0;
   end Measuring;

   ----------------------
   -- Read_Measurement --
   ----------------------

   procedure Read_Measurement
     (Device  : Device_Context;
      Trim    : Trim_Registers;
      Value   : out Density_Vector;
      Success : out Boolean)
   is
      Raw    : Raw_Density_Vector;
      R_Hall : BMM150.Raw_Hall;
      Hall   : Fixed_14_Unit;
      A0     : Fixed_14_Unit;
      XYZ1   : constant Fixed_14_Unit :=
        Fixed_14_Unit'Small * Positive (Trim.XYZ1);
   begin
      Read_Raw_Measurement (Device, Raw, R_Hall, Success);

      if Success and then R_Hall /= 0 then
         Hall := Fixed_14_Unit'Small * Positive (R_Hall);
         A0 := XYZ1 / Hall - 1.0;

         Compensate_XY (Raw.X, A0, Trim, Trim.X1, Trim.X2, Value.X);
         Compensate_XY (Raw.Y, A0, Trim, Trim.Y1, Trim.Y2, Value.Y);
         Compensate_Z (Raw.Z, R_Hall, Trim, Value.Z);
      else
         Value := (others => Density'First);
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
      use Interfaces;

      function Cast is new Ada.Unchecked_Conversion (Unsigned_16, Integer_16);

      function Decode (Data : Byte_Array) return Unsigned_16 is
         (Shift_Left (Unsigned_16 (Data (Data'Last)), 8) +
          Unsigned_16 (Data (Data'First)));

      function Decode (Data : Byte_Array; Pad : Positive) return Integer_16 is
         (Cast (Shift_Right_Arithmetic (Decode (Data), Pad)));

      Raw : Byte_Array (16#42# .. 16#49#);
   begin
      Read (Device, Raw, Success);

      if Success then
         Value :=
           (X => Decode (Raw (16#42# .. 16#43#), 3),
            Y => Decode (Raw (16#44# .. 16#45#), 3),
            Z => Decode (Raw (16#46# .. 16#47#), 1));

         R_Hall := Shift_Right (Decode (Raw (16#48# .. 16#49#)), 2);
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
      use type Byte;

      Data : Byte_Array (16#4B# .. 16#4B#);
   begin
      Write (Device, 16#4B#, 16#83#, Success);

      if not Success then
         return;
      end if;

      for J in 1 .. 300 loop
         Read (Device, Data, Success);

         if Success and then (Data (Data'First) and 16#82#) = 0 then
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
      use type Byte;
      Map : constant array (Byte range 1 .. 7) of Output_Data_Rate :=
        [2, 6, 8, 15, 20, 25, 30];
      Data : Byte := 0;
   begin
      if ODR /= 10 then
         for J in Map'Range loop
            if ODR = Map (J) then
               Data := J;
               exit;
            end if;
         end loop;
      end if;

      Data := Data * 4 + Power_Mode'Pos (Mode);
      Data := Data * 2;
      Write (Device, 16#4C#, Data, Success);
   end Set_Power_Mode;

end BMM150.Internal;
