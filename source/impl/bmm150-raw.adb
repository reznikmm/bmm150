--  SPDX-FileCopyrightText: 2025-2026 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
----------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body BMM150.Raw is

   ---------------------
   -- Get_Measurement --
   ---------------------

   function Get_Measurement
     (Raw  : Byte_Array;
      Trim : Trim_Registers) return Optional_Magnetic_Field_Vector
   is
      Data     : Raw_Density_Vector;
      R_Hall   : BMM150.Raw_Hall;
   begin
      Get_Raw_Measurement (Raw, Data, R_Hall);
      return To_Magnetic_Field_Vector (Data, R_Hall, Trim);
   end Get_Measurement;

   -------------------------
   -- Get_Raw_Measurement --
   -------------------------

   procedure Get_Raw_Measurement
     (Raw    : Byte_Array;
      Value  : out Raw_Density_Vector;
      R_Hall : out BMM150.Raw_Hall)
   is
      use Interfaces;

      function Cast is new Ada.Unchecked_Conversion (Unsigned_16, Integer_16);

      function Decode (Data : Byte_Array) return Unsigned_16 is
         (Shift_Left (Unsigned_16 (Data (Data'Last)), 8) +
          Unsigned_16 (Data (Data'First)));

      function Decode (Data : Byte_Array; Pad : Positive) return Integer_16 is
         (Cast (Shift_Right_Arithmetic (Decode (Data), Pad)));

   begin
      Value :=
        (X => Decode (Raw (16#42# .. 16#43#), 3),
         Y => Decode (Raw (16#44# .. 16#45#), 3),
         Z => Decode (Raw (16#46# .. 16#47#), 1));

      R_Hall := Shift_Right (Decode (Raw (16#48# .. 16#49#)), 2);
   end Get_Raw_Measurement;

   ------------------------
   -- Get_Trim_Registers --
   ------------------------

   function Get_Trim_Registers (Raw  : Byte_Array) return Trim_Registers is
      use Interfaces;

      function Cast_8 is new Ada.Unchecked_Conversion (Byte, Integer_8);

      function Cast is new Ada.Unchecked_Conversion
        (Unsigned_16, Integer_16);

      function To_Unsigned (LSB, MSB : Byte) return Unsigned_16 is
        (Unsigned_16 (LSB) + Shift_Left (Unsigned_16 (MSB), 8));

      function To_Integer (LSB, MSB : Byte) return Integer_16 is
        (Cast (To_Unsigned (LSB, MSB)));

      Z2 : Integer_16;

      Value : Trim_Registers;
   begin
      Value.X1 := Cast_8 (Raw (16#5D#));
      Value.Y1 := Cast_8 (Raw (16#5E#));

      Value.Z4 := To_Integer (Raw (16#62#), Raw (16#63#));
      Value.X2 := Cast_8 (Raw (16#64#));
      Value.Y2 := Cast_8 (Raw (16#65#));

      Value.Z1 := To_Unsigned (Raw (16#6A#), Raw (16#6B#));

      Z2 := To_Integer (Raw (16#68#), Raw (16#69#));
      --  Success := Z2 > 0;   ???
      Value.Z2 := (if Z2 > 0 then Z2 else 1);
      Value.Z3 := To_Integer (Raw (16#6E#), Raw (16#6F#));

      Value.XY1 := Raw (16#71#);
      Value.XY2 := Cast_8 (Raw (16#70#));

      Value.XYZ1 := To_Unsigned (Raw (16#6C#), Raw (16#6D#) and 127);

      return Value;
   end Get_Trim_Registers;

   --------------------
   -- Set_Power_Mode --
   --------------------

   function Set_Power_Mode
     (Mode : Power_Mode;
      ODR  : Output_Data_Rate) return Power_Mode_Data
   is
      Map : constant array (Byte range 1 .. 7) of Output_Data_Rate :=
        (2, 6, 8, 15, 20, 25, 30);

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

      return (Power_Mode_Data'First => Data);
   end Set_Power_Mode;

end BMM150.Raw;
