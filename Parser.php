<?php
namespace Assembler;

class Parser {

    private $file;

    private $command;

    public function __construct(string $fileName) {
        $this->file = file($fileName, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    }

    public function hasMoreCommands(): bool {
        return key($this->file) < count($this->file) - 1;
    }

    public function advance() {
        if (!$this->hasMoreCommands())
            return;
        $this->command = trim(next($this->file));
    }

    public function reset() {
        reset($this->file);
    }

    public function commandType(): int {
        switch ($this->command[0]) {
            case "@":
                return CommandType::A_COMMAND;
                break;
            case "(":
                return CommandType::L_COMMAND;
                break;
            case "M":
            case "D":
            case "A":
            case "0":
                return CommandType::C_COMMAND;
                break;
            default:
                return CommandType::NONE;
        }
    }

    public function symbol(): string {
        $commandType = $this->commandType();
        $matches = array();
        if ($commandType === CommandType::A_COMMAND) {
            preg_match("/^@(.+?)\s*$/", $this->command, $matches);
            return $matches[1];
        }
        else if ($commandType === CommandType::L_COMMAND) {
            preg_match("/^\((.+?)\)\s*$/", $this->command, $matches);
            return $matches[1];
        }
        throw new \Exception("symbol() called on C_COMMAND");
    }

    public function dest(): string {
        if ($this->commandType() !== CommandType::C_COMMAND)
            throw new \Exception("dest() called on non-C_COMMAND");
        $matches = array();
        preg_match("/^([AMD]{1,3})=/", $this->command, $matches);
        if (!empty($matches))
            return $matches[1];
        return "";
    }

    public function comp(): string {
        if ($this->commandType() !== CommandType::C_COMMAND)
            throw new \Exception("comp() called on non-C_COMMAND");
        $matches = array();
        preg_match("/(D[+\-&|]?[1AM]?|A[+\-][D1]|M[+\-][D1]$|[\-!]?[01AMD])(?:$|;|\s*\/\/)/", $this->command, $matches);
        if (!empty($matches))
            return $matches[1];
        return "";
    }

    public function jump(): string {
        if ($this->commandType() !== CommandType::C_COMMAND)
            throw new \Exception("jump() called on non-C_COMMAND");
        $matches = array();
        preg_match("/[0D];(J(?:G[ET]|EQ|L[TE]|NE|MP)|null)/", $this->command, $matches);
        if (!empty($matches))
            return $matches[1];
        return "";
    }
}