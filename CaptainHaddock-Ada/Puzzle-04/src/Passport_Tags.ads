package Passport_Tags is
    pragma Preelaborate;

    Tag_Length : constant Positive := 4;

    Tag_BYR : constant String (1 .. Tag_Length) := "byr:";
    Tag_IYR : constant String (1 .. Tag_Length) := "iyr:";
    Tag_EYR : constant String (1 .. Tag_Length) := "eyr:";
    Tag_HGT : constant String (1 .. Tag_Length) := "hgt:";
    Tag_HCL : constant String (1 .. Tag_Length) := "hcl:";
    Tag_ECL : constant String (1 .. Tag_Length) := "ecl:";
    Tag_PID : constant String (1 .. Tag_Length) := "pid:";
    Tag_CID : constant String (1 .. Tag_Length) := "cid:";

    type Passport_Info_list is (BYR, IYR, EYR, HGT, HCL, ECL, PID, CID);

    Tag_Value : constant array
       (Passport_Info_list) of String (1 .. Tag_Length) :=
       (BYR => Tag_BYR,
        IYR => Tag_IYR,
        EYR => Tag_EYR,
        HGT => Tag_HGT,
        HCL => Tag_HCL,
        ECL => Tag_ECL,
        PID => Tag_PID,
        CID => Tag_CID);

    Tag_NULL : constant String (1 .. Tag_Length) := "    ";

private

end Passport_Tags;
