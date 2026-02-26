--  SPDX-FileCopyrightText: 2023-2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

package body BMM150 is

   Small_14 : constant := 1.0 / 2.0 ** 14;
   type Fixed_14 is delta Small_14 range -2.0 ** 17 .. 2.0 ** 17 - Small_14;
   subtype Fixed_14_Unit is Fixed_14 range -1.0 + Small_14 .. 1.0 - Small_14;

   procedure Compensate_XY
     (X        : Raw_XY;
      A0       : Fixed_14_Unit;
      Reg      : Trim_Registers;
      X_1      : Interfaces.Integer_8;
      X_2      : Interfaces.Integer_8;
      Result   : out Magnetic_Field;
      Overflow : in out Boolean)
        with SPARK_Mode, Inline;
   --  Part of compensation procedure for X and Y

   procedure Compensate_Z
     (Z        : Raw_Z;
      Hall     : Raw_Hall;
      Reg      : Trim_Registers;
      Result   : out Magnetic_Field;
      Overflow : in out Boolean)
        with SPARK_Mode, Inline;
   --  Part of compensation procedure for Z

   -------------------
   -- Compensate_XY --
   -------------------

   procedure Compensate_XY
     (X        : Raw_XY;
      A0       : Fixed_14_Unit;
      Reg      : Trim_Registers;
      X_1      : Interfaces.Integer_8;
      X_2      : Interfaces.Integer_8;
      Result   : out Magnetic_Field;
      Overflow : in out Boolean)
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

      Field_Small : constant Fixed_16 := Magnetic_Field'Small;

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
      if A8 in
        Fixed_16 (Magnetic_Field'First) .. Fixed_16 (Magnetic_Field'Last)
      then
         Result := Magnetic_Field (A8);
      else
         Result := 0.0;
         Overflow := True;
      end if;
   end Compensate_XY;

   ------------------
   -- Compensate_Z --
   ------------------

   procedure Compensate_Z
     (Z        : Raw_Z;
      Hall     : Raw_Hall;
      Reg      : Trim_Registers;
      Result   : out Magnetic_Field;
      Overflow : in out Boolean)
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
      if A5 in Wide_Fixed_4 (Magnetic_Field'First) ..
        Wide_Fixed_4 (Magnetic_Field'Last)
      then
         Result := Magnetic_Field (A5);
      else
         Result := 0.0;
         Overflow := True;
      end if;
   end Compensate_Z;

   ------------------------------
   -- To_Magnetic_Field_Vector --
   ------------------------------

   function To_Magnetic_Field_Vector
     (Raw    : Raw_Density_Vector;
      R_Hall : BMM150.Raw_Hall;
      Trim   : Trim_Registers) return Optional_Magnetic_Field_Vector
   is
      Value    : Magnetic_Field_Vector;
      Overflow : Boolean := False;
      Hall     : Fixed_14_Unit;
      A0       : Fixed_14_Unit;
      XYZ1     : constant Fixed_14_Unit :=
        Fixed_14_Unit'Small * Positive (Trim.XYZ1);
   begin
      if R_Hall = 0 then
         return (0.0, 0.0, 0.0, Overflow => True);
      end if;

      Hall := Fixed_14_Unit'Small * Positive (R_Hall);
      A0 := XYZ1 / Hall - 1.0;

      Compensate_XY (Raw.X, A0, Trim, Trim.X1, Trim.X2, Value.X, Overflow);
      Compensate_XY (Raw.Y, A0, Trim, Trim.Y1, Trim.Y2, Value.Y, Overflow);
      Compensate_Z (Raw.Z, R_Hall, Trim, Value.Z, Overflow);

      return (Value.X, Value.Y, Value.Z, Overflow);
   end To_Magnetic_Field_Vector;

end BMM150;
