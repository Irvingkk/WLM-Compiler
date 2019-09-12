#include "scanner.h"
#include <iostream>
#include <stdio.h>
#include <map>
using std::vector;
using std::string;
using CS241::Token;

/**
 * C++ Starter code for CS241 A3
 * Written by Sean Harrap in Winter 2018
 * All code requires C++14, so if you're getting
 * compile errors make sure to use -std=c++14.
 *
 * This file contains the main function of your program.
 * You will want to begin by modifying the assemble() function,
 * and are welcome to add more functions and files as needed.
 */

// You can throw this class to signal exceptions encountered while assembling
class AssemblingFailure {
  std::string message;
  public:
    AssemblingFailure(std::string message):message(message) {}

    std::string getMessage() const { return message; }
};

void gen_code(int word){
  putchar((word >> 24) & 0xff);
  putchar((word >> 16) & 0xff);
  putchar((word >> 8) & 0xff);
  putchar(word & 0xff);
}

void assemble(vector<vector<Token>> &tokenLines) {
  std::map<string, int>st;
  int address =0;
// Pass 1
  for (vector<Token> &line : tokenLines) {
    // output
    //for (auto &token : line) {
    //  std::cout << "Token(" << token.getKind() << "," << token.getLexeme()
    //            << ") ";
    //}
    //std::cout << std::endl;

    // check if it's a blank line
    if(line.size() == 0)continue;
    // put the labels to map.
    int i = 0;
    while(i < line.size() && line[i].getKind() == "LABEL"){
      string label = line[i].getLexeme();
      label = label.substr(0, label.length() - 1);
      //std::cout << st.count(label)<< std::endl;
      if(st.count(label) != 0){
        throw AssemblingFailure("ERROR: LABEL has been defined");
      }
      st[label] = address;
      ++i;
    }
    // check if there are only labels
    if(line.size() == i) continue;

    // check if the remaining line is a valid inst
    // valid .word
    if(line[i].getKind() == "WORD") {
      if(line.size() - i != 2){
        throw AssemblingFailure("ERROR: incorrect # of args");
      }
      // check if .word has valid args
      Token &args = line[i + 1];
      string type = args.getKind();
      if(type != "INT" && type != "HEXINT" && type != "ID"){
        throw AssemblingFailure("ERROR: invalid arguments of .word");
      }
    }
    // valid other inst
    else if(line[i].getKind() == "ID"){
      string inst_type = (line[i].getLexeme());
      if(inst_type == "jr" || inst_type == "jalr"){
        if(line.size() - i != 2 || line[i + 1].getKind() != "REG"){
          throw AssemblingFailure("ERROR: invalid instruction arguments");
        }
      } else if(inst_type == "add" || inst_type == "sub" ||
              inst_type == "slt" || inst_type == "sltu"){
        if(line.size() - i != 6 || line[i + 1].getKind() != "REG" ||
           line[i + 2].getKind() != "COMMA" || line[i + 3].getKind() != "REG" ||
           line[i + 4].getKind() != "COMMA" || line[i + 5].getKind() != "REG"){
          throw AssemblingFailure("ERROR: invalid instruction arguments");
        }
      } else if(inst_type == "beq" || inst_type == "bne"){
        if(line.size() - i != 6 || line[i + 1].getKind() != "REG" ||
           line[i + 2].getKind() != "COMMA" || line[i + 3].getKind() != "REG" ||
           line[i + 4].getKind() != "COMMA") {
          throw AssemblingFailure("ERROR: invalid instruction arguments");
        }
        if(line[i + 5].getKind() == "INT"){
          int offset = line[i + 5].toLong();
          if(offset > 32767 || offset < -32768){
            throw AssemblingFailure("ERROR: offset beq/bne out of range");
          }
        }
        else if(line[i + 5].getKind() == "HEXINT"){
          string offset = line[i + 5].getLexeme();
          if(offset.size() > 6){
            throw AssemblingFailure("ERROR: offset beq/bne out of range");
          }
        }
        else if(line[i + 5].getKind() != "ID"){
          throw AssemblingFailure("ERROR: invalid instruction arguments");
        }
      } else if(inst_type == "lis" || inst_type == "mflo" || inst_type == "mfhi"){
        if(line.size() - i != 2 || line[i + 1].getKind() != "REG"){
          throw AssemblingFailure("ERROR: invalid instruction arguments");
        }
      } else if(inst_type == "mult" || inst_type == "multu" || inst_type == "div" ||
                inst_type == "divu"){
        if(line.size() - i != 4 || line[i + 1].getKind() != "REG"
            || line[i + 2].getKind() != "COMMA" || line[i + 3].getKind() != "REG"){
          throw AssemblingFailure("ERROR: invalid instruction arguments");
        }
      } else if(inst_type == "lw" || inst_type == "sw"){
        if(line.size() != 7 || line[i + 1].getKind() != "REG" ||
            line[i + 2].getKind() != "COMMA" || line[i + 4].getKind() != "LPAREN" ||
            line[i + 5].getKind() != "REG" || line[i + 6].getKind() != "RPAREN"){
          throw AssemblingFailure("ERROR: invalid instruction arguments");
        } else if(line[i + 3].getKind() == "INT"){
          int offset = line[i + 3].toLong();
          if(offset > 32767 || offset < -32768){
            throw AssemblingFailure("ERROR: offset lw/sw out of range");
          }
        } else if(line[i + 3].getKind() == "HEXINT"){
          string offset = line[i + 3].getLexeme();
          if(offset.size() > 6){
            throw AssemblingFailure("ERROR: offset lw/sw out of range");
          }
        } else{
          throw AssemblingFailure("ERROR: invalid instruction arguments");
        }
      } else { // Invalid ID
          throw AssemblingFailure("ERROR: invalid instruction ID");
      }
    }
    // Invalid ID
    else {
      throw AssemblingFailure("ERROR: invalid instruction ID");
    }
    address += 4;
  }

  // Pass 2
  address = 0;
  for(vector<Token> &line : tokenLines){
    int p; // the number which will be used to putchar
    int s = line.size();
    // empty line
    if(s == 0)continue;
    int i = 0;
    bool is_inst = false;
    while(i < s - 1){
      if(line[i].getKind() == "WORD"){ // IF INST IS .WORD
        if(line[i +1].getKind() == "HEXINT" || line[i +1].getKind() == "INT"){
          long n = line[i + 1].toLong();
          p = (int) n;
        } else {
          if(st.count(line[i + 1].getLexeme()) == 0){
            throw AssemblingFailure("ERROR: invalid arguments of .word");
          }
          p = st[line[i+1].getLexeme()];
        }
        gen_code(p);
        is_inst = true;
        break;
      } else if(line[i].getKind() == "ID"){ // IF IT'S OTHER INST
        int word;
        string inst_type = line[i].getLexeme();
        // "jr" or "jalr"
        if(inst_type == "jr"){
          word = 8 | (line[i + 1].toInt() << 21);
        } else if(inst_type == "jalr"){
          word = 9 | (line[i + 1].toInt() << 21);
        }
        // "add", "sub", "slt", "sltu"
        else if(inst_type == "add" || inst_type == "sub" ||
                  inst_type == "slt" || inst_type == "sltu"){
          word = (line[i + 3].toInt() << 21) | (line[i + 5].toInt() << 16)
                  | (line[i + 1].toInt() << 11);
          switch(inst_type[1]){
            case 'd':
              word = word | 32;
              break;
            case 'u':
              word = word | 34;
              break;
            default:
              if(inst_type == "slt"){
                word = word | 42;
              } else {
                word = word | 43;
              }
          }
        }
        // "beq" or "bne"
        else if(inst_type == "beq" || inst_type == "bne"){
          int offset;
          if(line[i + 5].getKind() == "ID"){
            if(st.count(line[i + 5].getLexeme()) == 0){
              throw AssemblingFailure("ERROR: invalid beq/bne offset");
            }
            offset = (st[line[i + 5].getLexeme()] - address - 4) / 4;
            if(offset > 32767 || offset < -32768){
              throw AssemblingFailure("ERROR: invalid beq/bne offset");
            }
          } else{
            offset = line[i + 5].toLong();
          }
          if(inst_type == "beq"){
            word = (4 << 26) | (line[i + 1].toInt() << 21) | (line[i + 3].toInt() << 16)
                   | (offset & 0xffff);
          } else {
            word = (5 << 26) | (line[i + 1].toInt() << 21) | (line[i + 3].toInt() << 16)
                   | (offset & 0xffff);
          }
        }
        // "lis", "mflo", "mfhi"
        else if(inst_type == "lis"){
          word = (line[i + 1].toInt() << 11) | 20;
        } else if(inst_type == "mflo"){
          word = (line[i + 1].toInt() << 11) | 18;
        } else if(inst_type == "mfhi"){
          word = (line[i + 1].toInt() << 11) | 16;
        }
        // "mult", "multu", "div", "divu"
        else if(inst_type == "mult"){
          word = (line[i + 1].toInt() << 21) | (line[i + 2].toInt() << 16) | 24;
        } else if(inst_type == "multu"){
          word = (line[i + 1].toInt() << 21) | (line[i + 2].toInt() << 16) | 25;
        } else if(inst_type == "div"){
          word = (line[i + 1].toInt() << 21) | (line[i + 2].toInt() << 16) | 26;
        } else if(inst_type == "divu"){
          word = (line[i + 1].toInt() << 21) | (line[i + 2].toInt() << 16) | 27;
        }
        // "lw" or "sw"
        else if(inst_type == "lw"){
          int offset = line[i + 3].toLong();
          word = (35 << 26) | (line[i + 1].toInt() << 16) | (line[i + 5].toInt() << 21
                  | (offset & 0xffff));
        } else if(inst_type == "sw"){
          int offset = line[i + 3].toLong();
          word = (43 << 26) | (line[i + 1].toInt() << 16) | (line[i + 5].toInt() << 21
                  | (offset & 0xffff));
        }
        gen_code(word);
        is_inst = true;
        break;
      } // finish ID block
      ++i;
    } // finish scan line loop
    if(is_inst == true){
      address += 4;
    }
  } // finish iteration for one line

  // output symbol table to standard error
  //for(std::map<string, int>::iterator it = st.begin(); it != st.end(); ++it){
  //  std::cerr << it->first << " " << it->second <<std::endl;
  //
}





// Convert the input into a sequence of tokens.
// This should not need to be modified, but you can if you want to.
int main() {
  CS241::AsmDFA theDFA;
  vector<vector<Token>> tokenLines;
  string line;

  try {
    while (getline(std::cin, line)) {
      tokenLines.push_back(theDFA.scan(line));
    }
  } catch (CS241::ScanningFailure &f) {
    std::cerr << f.getMessage() << std::endl;

    return 1;
  }

  try {
    assemble(tokenLines);
  } catch (AssemblingFailure &f) {
    std::cerr << f.getMessage() << std::endl;

    return 1;
  }

  return 0;
}
