<?php
namespace Assembler;


class SymbolTable{

    public const RESERVED = array(
        "SP" => 0,
        "LCL" => 1,
        "ARG" => 2,
        "THIS" => 3,
        "THAT" => 4,
        "R0" => 0,
        "R1" => 1,
        "R2" => 2,
        "R3" => 3,
        "R4" => 4,
        "R5" => 5,
        "R6" => 6,
        "R7" => 7,
        "R8" => 8,
        "R9" => 9,
        "R10" => 10,
        "R11" => 11,
        "R12" => 12,
        "R13" => 13,
        "R14" => 14,
        "R15" => 15,
        "SCREEN" => 16384,
        "KBD" => 24576,
    );

    private $SymbolTable;

    public function __construct() {
        $this->SymbolTable = array();
    }

    public function addEntry(string $symbol, int $address) {
        $this->SymbolTable[$symbol] = $address;
    }

    public function contains(string $symbol): bool {
        return array_key_exists($symbol, $this->SymbolTable);
    }

    public function GetAddress(string $symbol): int {
        return $this->SymbolTable[$symbol];
    }
}