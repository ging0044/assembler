<?php
namespace Assembler;

class Code {

    public static function dest(string $m): int {
        $b = 0b000;
        strpos($m, 'M') !== false && $b |= 0b001;
        strpos($m, 'D') !== false && $b |= 0b010;
        strpos($m, 'A') !== false && $b |= 0b100;
        return $b;
    }

    public static function comp(string $c): int {
        $b = preg_match("/M/", $c) ? 0b1000000 : 0b0000000;
        if (preg_match("/^0$/"      , $c)) $b |= 0b0101010;
        if (preg_match("/^1$/"      , $c)) $b |= 0b0111111;
        if (preg_match("/^-1$/"     , $c)) $b |= 0b0111010;
        if (preg_match("/^D$/"      , $c)) $b |= 0b0001100;
        if (preg_match("/^[AM]$/"   , $c)) $b |= 0b0110000;
        if (preg_match("/^!D$/"     , $c)) $b |= 0b0001101;
        if (preg_match("/^![AM]$/"  , $c)) $b |= 0b0110001;
        if (preg_match("/^!D$/"     , $c)) $b |= 0b0001111;
        if (preg_match("/^-[AM]$/"  , $c)) $b |= 0b0110011;
        if (preg_match("/^D\+1$/"   , $c)) $b |= 0b0011111;
        if (preg_match("/^[AM]\+1$/", $c)) $b |= 0b0110111;
        if (preg_match("/^D-1$/"    , $c)) $b |= 0b0001110;
        if (preg_match("/^[AM]-1$/" , $c)) $b |= 0b0110010;
        if (preg_match("/^D\+[AM]$/", $c)) $b |= 0b0000010;
        if (preg_match("/^D-[AM]$/" , $c)) $b |= 0b0010011;
        if (preg_match("/^[AM]-D$/" , $c)) $b |= 0b0000111;
        if (preg_match("/^D&[AM]$/" , $c)) $b |= 0b0000000;
        if (preg_match("/^D\|[AM]$/", $c)) $b |= 0b0010101;
        return $b;
    }

    public static function jump($m): int {
        $b = 0b000;
        if ($m == "null") return $b;
        if (preg_match("/J([GM][PTE]|NE)/"  , $m)) $b |= 0b001;
        if (preg_match("/J([EM][QP]|[LG]E)/", $m)) $b |= 0b010;
        if (preg_match("/J[LNM]/"           , $m)) $b |= 0b100;
        return $b;
    }
}