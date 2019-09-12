#include <memory>
#include <iostream>
#include <set>
#include <string>
#include <sstream>
#include <algorithm>
#include <climits>
#include <map>
#include <typeinfo>
#include <vector>

using namespace std;

struct Node{
  map<string, int>* symtbl;
  set<Node *> children;
  vector<string> code;
  string type;
  string lexeme;

  Node(string type, string lexeme): type(type), lexeme(lexeme), symtbl(NULL){}
  bool isleaf(){
    return children.empty();
  }
  friend class Tree;
};

string push_var(string reg){
  string comment = ";; push " + reg + "\n";
  string pushreg = "sw "+ reg +", -4($30)\n";
  return comment + pushreg + "sub $30, $30, $4\n";
}
string pop_var(string reg){
  string comment = ";; pop to " + reg + "\n";
  string update = "add $30, $30, $4\n";
  return comment + update + "lw "+ reg+ ", -4($30)\n";
}

string getbegin(string line){
  istringstream iss{line};
  string begin;
  iss >> begin;
  return begin;
}
string getsecond(string line){
  istringstream iss{line};
  string part;
  iss >> part;
  iss >> part;
  return part;
}

class Tree{
  set<string> terminals;
  Node* tree;
  void readterminal(){
    terminals.insert("BECOMES");
    terminals.insert("BOF");
    terminals.insert("COMMA");
    terminals.insert("ELSE");
    terminals.insert("EOF");
    terminals.insert("EQ");
    terminals.insert("GE");
    terminals.insert("GT");
    terminals.insert("ID");
    terminals.insert("IF");
    terminals.insert("INT");
    terminals.insert("LBRACE");
    terminals.insert("LE");
    terminals.insert("LPAREN");
    terminals.insert("LT");
    terminals.insert("MINUS");
    terminals.insert("NE");
    terminals.insert("NUM");
    terminals.insert("PCT");
    terminals.insert("PLUS");
    terminals.insert("RBRACE");
    terminals.insert("RETURN");
    terminals.insert("RPAREN");
    terminals.insert("SEMI");
    terminals.insert("SLASH");
    terminals.insert("STAR");
    terminals.insert("WHILE");
  }
  int countelements(string rule){// count the elements of the RHS of a rule
    string line;
    istringstream iss{rule};
    int count = 0;
    while(iss >> line){
      count ++;
    }
    count--;
    return count;
  }
  Node* build_tree(){
    string line;
    getline(cin, line);
    if(terminals.find(getbegin(line)) != terminals.end()){// line is a terminal
      Node* leaf = new Node(getbegin(line), getsecond(line));
      for(int i = 0; i <= leaf->children.size(); ++i){
        leaf->code.push_back("");
      }
      return leaf;
    } else{ // line is a rule
      Node* node= new Node(getbegin(line), "");
      int child = countelements(line);
      for(int i = 0; i < child; ++i){
        Node* child = build_tree();
        node->children.insert(child);
      }
      for(int i = 0; i <= node->children.size(); ++i){
        node->code.push_back("");
      }
      return node;
    }
  }
  void build(){
    tree = build_tree();
  }

  void delete_tree(Node* node){ // this function is used to delete_tree.
    if(node->children.empty()){
      return;
    }
    if(node->type == "procedure" && node->children.size() == 12){
      delete node->symtbl;
      node->symtbl = NULL;
    }
    for(Node* child : node->children){
      delete_tree(child);
      delete child;
      child = NULL;
    }
  }

  void print(Node* node){
    // if(node->isleaf()){
    //   cout << node->type << " "<< node->lexeme << endl;
    //   return;
    // }
    // cout << node->type << endl;
    // set<Node *> children= node->children;
    // for(Node* child : children){
    //   print(child);
    // }
  }

  void prints(Node* node){
    if(node->isleaf()){
      cout << node->code[0];
      return;
    } else{
      cout << node->code[0];
      int n = 1;
      for(Node* child : node->children){
        //cout << n << endl;
        prints(child);
        cout << node->code[n];
        ++n;
      }
    }
  }

public:
  Tree(){
    readterminal();
    build();
  }
  Node* getTree(){
    return tree;
  }
  void print_tree(){
    print(tree);
  }
  void print_code(){
    prints(tree);
  }
  ~Tree(){
    delete_tree(tree);
    delete tree;
    tree = NULL;
  }
};

// pass 1 build symtbl and check syntatic error
void scan(Node* node, map<string, int>* symtbl, map<string, int> &procsymtbl){
  if(node->isleaf()){ // base case
    node->symtbl = symtbl;
    // cout << "base case" << endl;
    return;
  } else if(node->type == "start"){
    procsymtbl.insert(pair<string, int>("Fputchar", 1));
    procsymtbl.insert(pair<string, int>("Fgetchar", 0));
    for(Node* child : node->children){
      scan(child, symtbl, procsymtbl);
    }
  } else if(node->type == "procedure"){
    if(node->children.size() == 12){ // pass wain symtbl to children
      // cout << "procedure" << endl;
      node->symtbl = new map<string, int>;

      string fname;
      int params;
      for(Node* child : node->children){
        if(child->type == "ID"){
          fname = "F" + child->lexeme;
        }
        if(child->type == "params"){
          if(child->children.size() == 1){
            params = 1;
          } else if(child->children.size() == 3){
            params = 2;
          } else {
            params = 0;
          }
        }
      }
      if(procsymtbl.find(fname) != procsymtbl.end()){
        string error = "ERROR: procedure is duplicated defined";
        throw error;
      }
      procsymtbl.insert(pair<string, int>(fname, params));
      for(Node* child : node->children){
        scan(child, node->symtbl, procsymtbl);
      }
    } else {
      node->symtbl = symtbl;
      for(Node* child : node->children){
        scan(child, symtbl, procsymtbl);
      }
    }
  } else {
    node->symtbl = symtbl;
    for(Node* child : node->children){
      scan(child, symtbl, procsymtbl);
    }
  }
}


// pass 2 gencode
void gencode(Node* node, string& id, string& num, map<string, int> &procsymtbl){
  static int offset = -4;
  static int endloop = 0;
  static int loop = 0;
  static int ELSE = 0;
  static int endif = 0;
  // base case
  if(node->isleaf() && node->type != "NUM"){
    // cout << "base case" << endl;
    return;
  } else if(node->type == "start"){ // add prologue at the beginning
    // cout << "start" << endl;
    string prologue = ";; ========== prologue for the program ==========\n";
    prologue += "lis $11\n";
    prologue += ".word 1\n";
    prologue += "lis $4\n";
    prologue += ".word 4\n";
    node->code[0] = prologue + ";; jump to wain function\nbeq $0, $0, Fwain\n";
    for(Node* child : node->children){
      gencode(child, id, num, procsymtbl);
    }
  } else if(node->type == "procedure"){
    if(node->children.size() == 12){
      offset = -4;
      string fname;
      int params;

      // add prologue for wain to node
      string prologue = ":\n" + push_var("$31")+ push_var("$29") + push_var("$5") + push_var("$6")
              + "add $29, $30, $0\n";

      for(Node* child : node->children){
        if(child->type == "ID"){
          fname = "F" + child->lexeme;
        }
        if(child->type == "params"){
          if(child->children.size() == 1){
            params == 1;
            prologue += push_var("$1");
          } else if(child->children.size() == 3){
            params == 2;
            prologue += push_var("$1") + push_var("$2");
          } else {
            params == 0;
          }
        }
      }
      prologue = fname + prologue;
      string comment = ";;======== prologue for " + fname + " ========\n";
      prologue += ";; ========= main code for " + fname + " ==========\n";
      // add prologue to code
      node->code[0] = comment + prologue;

      if(fname == "Fwain"){ // wain function
        string put_char = "Fputchar:\n" + push_var("$3") + "lis $3\n.word 0xffff000c\n";
        put_char += "sw $1, 0($3)\n" + pop_var("$3") + "jr $31\n";
        string get_char = "Fgetchar:\n";
        get_char += "lis $3\n.word 0xffff0004\nlw $3, 0($3)\njr $31\n";

        string epilogue = "epilogue:\nadd $30, $29, $0\n" + pop_var("$6") + pop_var("$5") + pop_var("$29")
                + pop_var("$31") + "jr $31\n";
        node->code[12] = "beq $0, $0, epilogue\n;; ========= epilogue for " + fname + " ==========\n"
                + put_char + get_char + epilogue;
      } else { // other helper functions
        string epilogue = "add $30, $29, $0\n" + pop_var("$6") + pop_var("$5") + pop_var("$29")
                + pop_var("$31") + "jr $31\n";
        node->code[12] = ";; ========= epilogue for " + fname + " ==========\n" + epilogue;
      }
      for(Node* child : node->children){
        gencode(child, id, num, procsymtbl);
      }
    } else {
      for(Node* child : node->children){
        gencode(child, id, num, procsymtbl);
      }
    }
  } else if(node->type == "expr"){ // expr node
    if(node->children.size() == 1){
      gencode(*(node->children.begin()), id, num, procsymtbl);
    } else {
      // cout << "plus, minus" << endl;
      node->code[1] = push_var("$3");
      for(Node* child : node->children){
        if(child->type == "PLUS"){
          node->code[3] = pop_var("$5") + "add $3, $5, $3\n";
        } else if(child->type == "MINUS"){
          node->code[3] = pop_var("$5") + "sub $3, $5, $3\n";
        }
      }
      for(Node* child : node->children){
        gencode(child, id, num, procsymtbl);
      }
    }
  } else if(node->type == "term"){ // term node
    if(node->children.size() == 1){
      gencode(*(node->children.begin()), id, num, procsymtbl);
    } else {
      // cout << "star slash or pct" << endl;
      node->code[1] = push_var("$3");
      for(Node* child : node->children){
        if(child->type == "STAR"){
          node->code[3] += pop_var("$5") + "mult $5, $3\n";
          node->code[3] += "mflo $3\n";
        } else if(child->type == "SLASH"){
          node->code[3] += pop_var("$5") + "div $5, $3\n";
          node->code[3] += "mflo $3\n";
        } else if(child->type == "PCT"){
          node->code[3] += pop_var("$5") + "div $5, $3\n";
          node->code[3] += "mfhi $3\n";
        }
      }
      for(Node* child : node->children){
        gencode(child, id, num, procsymtbl);
      }
    }
  } else if(node->type == "factor"){ // factor node
    for(Node* child : node->children){
      if(child->type == "ID"){
        //cout << "ID" << endl;
        map<string, int>::iterator it = (node->symtbl)->find(child->lexeme);
        if(it == (node->symtbl)->end()){
          string error = "ERROR: " + child->lexeme +" doesn't exist";
          throw(error);
        }
        int off = it->second;
        node->code[0] = "lw $3, " + to_string(off) + "($29)\n";
        //id = child->lexeme;
        return;
      }
    }
    for(Node* child : node->children){
      gencode(child, id, num, procsymtbl);
    }
  } else if(node->type == "pcall"){
    string fname;
    string ID;
    for(Node*child : node->children){
      if(child->type == "ID"){
        ID = child->lexeme;
        fname = "F" + child->lexeme;
      }
    }
    if((node->symtbl)->count(ID) == 1){
      string error = "ERROR";
      throw error;
    }
    int params = procsymtbl[fname];
    if(node->children.size() == 3){ // pcall for function with NO argument
      if(params != 0 || procsymtbl.find(fname) == procsymtbl.end()){
        string error = "ERROR: " + fname + " was not defined";
        throw error;
      }
      string pcall = "lis $31\n.word " + fname + "\njalr $31\n";
      node->code[0] = pcall;
    } else if(node->children.size() == 4){ // pcall for function with ONE argument
      if(params != 1 || procsymtbl.find(fname) == procsymtbl.end()){
        string error = "ERROR: " + fname + " was not defined";
        throw error;
      }
      string pcall = "add $1, $3, $0\nlis $31\n.word " + fname + "\njalr $31\n";
      node->code[3] = pcall;
    } else { // pcall for function with TWO arguments
      if(params != 2 || procsymtbl.find(fname) == procsymtbl.end()){
        string error = "ERROR: " + fname + " was not defined";
        throw error;
      }
      string pcall = "add $2, $3, $0\nlis $31\n.word " + fname + "\njalr $31\n";
      node->code[3] = "add $1, $3, $0\n";
      node->code[5] = pcall;
    }
    for(Node* child : node->children){
      gencode(child, id, num, procsymtbl);
    }
  } else if(node->type == "NUM"){
    long long int n;
    istringstream ss{node->lexeme};
    ss >> n;
    if(n > INT_MAX || n < INT_MIN){
      string error = "ERROR: num out of range";
      throw error;
    }
    node->code[0] = "lis $3\n.word " + node->lexeme + "\n";
    return;
  } else if(node->type == "dcls"){
    node->code[4]= push_var("$3");
    for(Node* child : node->children){
      gencode(child, id, num, procsymtbl);
    }
  } else if(node->type == "dcl"){ // params declaration
    // cout << "declaration" << endl;
    Node* ID = (*(node->children.rbegin()));
    // cout << ID->lexeme << endl;
    // cout << symtbl->count(ID->lexeme) << endl;
    string function = "F" + ID->lexeme;
    if((node->symtbl)->count(ID->lexeme) == 1){
      string error = "ERROR: duplicate defined";
      throw error;
    }
    (node->symtbl)->insert(pair<string, int>(ID->lexeme, offset));
    //cout << (node->symtbl)->count(ID->lexeme) << endl;
    //cout << ID->lexeme << endl;
    offset -= 4;
    return;
  } else if(node->type == "statement"){
    if(node->children.size() == 4){ // assignment statements
      for(Node* child : node->children){
        if(child->type == "ID"){
          if((node->symtbl)->count(child->lexeme) == 0){
            string error = "ERROR: " + child->lexeme + " was not defined!";
            throw error;
          }
          int off = ((node->symtbl)->find(child->lexeme))->second;
          node->code[3]= "sw $3, " + to_string(off) + "($29)\n";
        }
      }
      for(Node* child : node->children){
        gencode(child, id, num, procsymtbl);
      }
      return;
    } else if(node->children.size() == 7){ // while statement
      node->code[0] = "loop" + to_string(loop) + ":\n";
      node->code[3] = "beq $3, $0, endloop" + to_string(endloop) + "\n";
      node->code[6] = "beq $0, $0, loop" + to_string(loop) +"\nendloop" + to_string(endloop) + ":\n";
      loop++;
      endloop++;
      for(Node* child : node->children){
        gencode(child, id, num, procsymtbl);
      }
    } else if(node->children.size() == 11){ // if-else statement
      node->code[3] = "beq $3, $0, else" + to_string(ELSE) +"\n";
      node->code[6] = "beq $0, $0, endif" + to_string(endif) +"\nelse" +
                      to_string(ELSE) + ":\n";
      node->code[11] = "endif" + to_string(endif) + ":\n";
      ELSE++;
      endif++;
      for(Node* child : node->children){
        gencode(child, id, num, procsymtbl);
      }
    } else {
      for(Node* child : node->children){
        gencode(child, id, num, procsymtbl);
      }
    }
  } else if(node->type == "test"){
    node->code[1] = push_var("$3");
    node->code[3] = pop_var("$5");
    for(Node* child : node->children){
      if(child->type == "LT"){
        node->code[3] += "slt $3, $5, $3\n";
      } else if(child->type == "GT"){
        node->code[3] += "slt $3, $3, $5\n";
      } else if(child->type == "LE"){
        node->code[3] += "slt $3, $3, $5\nsub $3, $11, $3\n";
      } else if(child->type == "GE"){
        node->code[3] += "slt $3, $5, $3\nsub $3, $11, $3\n";
      } else if(child->type == "EQ"){
        node->code[3] += "sub $3, $3, $5\nbeq $3, $0, 1\nadd $3, $11, $0\nsub $3, $11, $3\n";
      } else if(child->type == "NE"){
        node->code[3] += "sub $3, $3, $5\nbeq $3, $0, 1\nadd $3, $11, $0\n";
      }
    }
    for(Node* child : node->children){
      gencode(child, id, num, procsymtbl);
    }
  }else {
    //cout << node->type << endl;
    for(Node* child : node->children){
      gencode(child, id, num, procsymtbl);
    }
  }
}

int main(){
  map<string, int> procsymtbl;
  Tree tree;
  Node* root = tree.getTree();
  //tree.print_tree();
  try {
    string num = "";
    string id = "";
    scan(root, NULL, procsymtbl);
    gencode(root, id, num, procsymtbl);
  } catch(string s){
    cerr << s << endl;
  }
  tree.print_code();
  return 0;
}
