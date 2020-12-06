module Day4Spec ( testInvalidPassports
                , testValidPassports
                ) where

import           Day4

import           Test.Tasty
import           Test.Tasty.HUnit

invalidPassports = [ "eyr:1972 cid:100\n hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
                   , "iyr:2019\n hcl:#602927 eyr:1967 hgt:170cm\n ecl:grn pid:012533040 byr:1946"
                   , "hcl:dab227 iyr:2012\n ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
                   , "hgt:59cm ecl:zzz\n eyr:2038 hcl:74454a iyr:2023\n pid:3556412378 byr:2007"
                   , "iyr:2019\n cid:314\n eyr:2039 hcl:#cfa07d hgt:171cm ecl:#0180ce byr:2006 pid:8204115568"
                   , "hcl:231d64 cid:124 ecl:gmt eyr:2039\n hgt:189in\n pid:#9c3ea1"
                   , "ecl:#1f58f9\n pid:#758e59\n iyr:2022\n hcl:z\n byr:2016 hgt:68 eyr:1933"
                   , "hgt:95 byr:1965\n pid:810311252 eyr:2034 hcl:z iyr:1985 cid:254"
                   , "pid:157cm cid:277\n iyr:1976 hgt:159in hcl:#341e13 ecl:#6c7644 eyr:2029 byr:1965"
                   , "pid:917105765 eyr:2021 hgt:181cm iyr:2019 cid:159 byr:1995\n ecl:gry"
                   , "byr:1983 iyr:2017\n pid:796082981 cid:129 eyr:2030\n ecl:oth hgt:182cm"
                   , "pid:188cm byr:2005\n hgt:170cm cid:163 ecl:#a08502 hcl:2964fb eyr:1994\n iyr:2005"
                   , "hcl:z eyr:1996 iyr:1993\n pid:#50f768\n ecl:zzz hgt:62cm byr:2017"
                   , "pid:320348494 iyr:2018 cid:281\n byr:2004\n hcl:#06a58b\n eyr:2033\n ecl:zzz\n hgt:76cm"
                   , "hgt:70cm byr:2015 pid:#218eb5 hcl:#0ec4fe iyr:2014 cid:228 ecl:#c8533a\n eyr:2035"
                   , "iyr:2019 ecl:oth\n hcl:#fffffd hgt:172cm pid:215010680\n eyr:2025"
                   , "pid:491710153 iyr:2012 ecl:#c85046 hcl:#b6652a\n eyr:2040 hgt:175cm byr:1981"
                   , "cid:239\n byr:1960 ecl:hzl\n hgt:164cm\n hcl:#51040b iyr:2018 eyr:2025"
                   , "ecl:brn\n byr:1982 iyr:2010 eyr:2029 pid:535752324 hcl:#efcc98"
                   , "iyr:2011 byr:1964 hgt:83 ecl:grn hcl:#c0946f pid:931162400 eyr:2028"
                   ]

validPassports = [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n hcl:#623a2f"
                 , "eyr:2029 ecl:blu cid:129 byr:1989\n iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
                 , "hcl:#888785\n hgt:164cm byr:2001 iyr:2015 cid:88\n pid:545766238 ecl:hzl\n eyr:2022"
                 ]

passportIsValid = isEntryValid2 . parseFields
passportIsInvalid = not . passportIsValid

testInvalidPassports = testGroup "Check invalid passports" $
    map (\i -> testCase "test case" $ passportIsInvalid i @? "case must be invalid: " ++ i) invalidPassports

testValidPassports = testGroup "Check invalid passports" $
    map (\i -> testCase "test case" $ passportIsValid i @? "case must be invalid") validPassports
