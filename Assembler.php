<?php
namespace Assembler;

require "Parser.php";
require "CommandType.php";
require "Code.php";
require "SymbolTable.php";

if (!isset($argv[1])) throw new \Exception("No input file specified");
$input = $argv[1];
$output = $argv[2] ?? explode(".", array_slice(explode("/", $input), -1)[0])[0].".hack";

$parser = new Parser($input);
$symbolTable = new SymbolTable();

/* INITIALIZE SYMBOL TABLE */
foreach(SymbolTable::RESERVED as $symbol => $address) {
    $symbolTable->addEntry($symbol, $address);
};
$currentInstruction = 0;

/* FIRST PASS */
while ($parser->hasMoreCommands()) {
    $parser->advance();

    switch($parser->commandType()) {
        case CommandType::A_COMMAND:
        case CommandType::C_COMMAND:
            $currentInstruction++;
            continue;
        case CommandType::NONE:
            continue;
        case CommandType::L_COMMAND:
            $symbolTable->addEntry($parser->symbol(), $currentInstruction);
            continue;
    }
}

$parser->reset();
$output = fopen($output, "wb");
$currentMemory = 16;

/* SECOND PASS */
while ($parser->hasMoreCommands()) {
    $parser->advance();

    switch ($parser->commandType()) {
        case CommandType::A_COMMAND:
            $symbol = $parser->symbol();
            if (!is_numeric($symbol)) {
                if ($symbolTable->contains($symbol)) {
                    $symbol = $symbolTable->GetAddress($symbol);
                }
                else {
                    $symbolTable->addEntry($symbol, $currentMemory);
                    $symbol = $currentMemory;
                    $currentMemory++;
                }
            }
            $symbol = decbin($symbol);
            $symbol = str_pad($symbol, 16, '0', STR_PAD_LEFT);
            fwrite($output, $symbol.PHP_EOL);
            continue;
        case CommandType::C_COMMAND:
            $jump = str_pad(decbin(Code::jump($parser->jump())), 3, '0', STR_PAD_LEFT);
            $dest = str_pad(decbin(Code::dest($parser->dest())), 3, '0', STR_PAD_LEFT);
            $comp = str_pad(decbin(Code::comp($parser->comp())), 7, '0', STR_PAD_LEFT);
            $b = str_pad($comp.$dest.$jump, 16, '1', STR_PAD_LEFT);
            fwrite($output, $b.PHP_EOL);
            continue;
        case CommandType::L_COMMAND:
        case CommandType::NONE:
            continue;
    }
}

fclose($output);