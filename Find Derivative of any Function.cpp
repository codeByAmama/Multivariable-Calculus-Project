#include<iostream>
#include<string>
#include<memory>
#include<stdexcept>
#include<cctype>
#include<cmath>
#include<sstream>
#include<iomanip>
#include<vector>
#include<algorithm>
using namespace std;
enum class Kind{ 
            Number, 
            Variable, 
			Add, 
			Sub, 
			Mul, 
			Div, 
			Pow, 
			Neg, 
			Sin, 
			Cos, 
			Tan, 
			Exp, 
			Ln 
	    };
struct Node{
    Kind kind;
    double value;
    string name;
    shared_ptr<Node> left, right;

    Node(Kind k, double v = 0.0, string n = "", shared_ptr<Node> l = nullptr, shared_ptr<Node> r = nullptr): kind(k), value(v), name(std::move(n)), left(std::move(l)), right(std::move(r)) {
	
	}
};
using Expr = shared_ptr<Node>;

Expr num(double v){ 
    return make_shared<Node>(Kind::Number, v); 
	}
Expr var(const string& s){ 
    return make_shared<Node>(Kind::Variable, 0, s); 
	}
Expr unary(Kind k, Expr a){ 
    return make_shared<Node>(k, 0, "", a);
	}
Expr binary(Kind k, Expr a, Expr b){ 
    return make_shared<Node>(k, 0, "", a, b);
	}

bool isNum(const Expr& e, double v){
    return e && e->kind == Kind::Number && fabs(e->value - v) < 1e-12;
}

string numberText(double v){
    if (fabs(v) < 1e-12)
	v = 0;
    ostringstream out;
    out << setprecision(12) << v;
    return out.str();
}

string printExpr(const Expr& e){
    if (!e) return "?";
    switch (e->kind){
        case Kind::Number: 
		return numberText(e->value);
        case Kind::Variable: 
		return e->name;
        case Kind::Add: 
		return "(" + printExpr(e->left) + "+" + printExpr(e->right) + ")";
        case Kind::Sub: 
		return "(" + printExpr(e->left) + "-" + printExpr(e->right) + ")";
        case Kind::Mul: 
		return "(" + printExpr(e->left) + "*" + printExpr(e->right) + ")";
        case Kind::Div: 
		return "(" + printExpr(e->left) + "/" + printExpr(e->right) + ")";
        case Kind::Pow: 
		return "(" + printExpr(e->left) + "^" + printExpr(e->right) + ")";
        case Kind::Neg: 
		return "(-" + printExpr(e->left) + ")";
        case Kind::Sin: 
		return "sin(" + printExpr(e->left) + ")";
        case Kind::Cos: 
		return "cos(" + printExpr(e->left) + ")";
        case Kind::Tan: 
		return "tan(" + printExpr(e->left) + ")";
        case Kind::Exp: 
		return "exp(" + printExpr(e->left) + ")";
        case Kind::Ln:  
		return "ln(" + printExpr(e->left) + ")";
    }
    return "?";
}

Expr simplify(const Expr& e) {
    if (!e) 
	return e;
    if (e->kind == Kind::Number || e->kind == Kind::Variable) 
	return e;

    Expr a = simplify(e->left);
    Expr b = e->right ? simplify(e->right) : nullptr;

    if(e->kind == Kind::Neg){
        if (isNum(a, 0)) 
		return num(0);
        if (a->kind == Kind::Number) 
		return num(-a->value);
        if (a->kind == Kind::Neg) 
		return a->left;
        return unary(Kind::Neg, a);
    }

    if(e->kind == Kind::Sin || e->kind == Kind::Cos || e->kind == Kind::Tan || e->kind == Kind::Exp || e->kind == Kind::Ln){
        if(a->kind == Kind::Number){
            double x = a->value;
            if (e->kind == Kind::Sin) 
			return num(sin(x));
            if (e->kind == Kind::Cos) 
			return num(cos(x));
            if (e->kind == Kind::Tan) 
			return num(tan(x));
            if (e->kind == Kind::Exp) 
			return num(exp(x));
            if (e->kind == Kind::Ln && x > 0) 
			return num(log(x));
        }
        return unary(e->kind, a);
    }

    if(a->kind == Kind::Number && b->kind == Kind::Number){
        double x = a->value, y = b->value;
          switch (e->kind) {
            case Kind::Add: 
			return num(x + y);
            case Kind::Sub: 
			return num(x - y);
            case Kind::Mul: 
			return num(x * y);
            case Kind::Div: 
			if (fabs(y) > 1e-12) 
			return num(x / y); 
			break;
            case Kind::Pow:
            if (x >= 0 || fabs(y - round(y)) < 1e-12) 
			return num(pow(x, y));
            break;
            default: 
			break;
        }
    }

    switch (e->kind){
        case Kind::Add:
            if(isNum(a, 0)) 
			return b;
            if(isNum(b, 0)) 
			return a;
            break;
        case Kind::Sub:
            if (isNum(b, 0)) 
			return a;
            if (isNum(a, 0)) 
			return unary(Kind::Neg, b);
            if (printExpr(a) == printExpr(b)) 
			return num(0);
            break;
        case Kind::Mul:
            if(isNum(a, 0) || isNum(b, 0)) 
			return num(0);
            if(isNum(a, 1)) 
			return b;
            if(isNum(b, 1)) 
			return a;
            if(isNum(a, -1)) 
			return unary(Kind::Neg, b);
            if(isNum(b, -1)) 
			return unary(Kind::Neg, a);
            break;
        case Kind::Div:
            if (isNum(a, 0) && !isNum(b, 0)) 
			return num(0);
            if (isNum(b, 1)) 
			return a;
            if (printExpr(a) == printExpr(b) && a->kind != Kind::Number) 
			return num(1);
            break;
        case Kind::Pow:
            if(isNum(b, 0)) 
			return num(1);
            if(isNum(b, 1)) 
			return a;
            if(isNum(a, 1)) 
			return num(1);
            break;
        default: 
		     break;
    }
    return binary(e->kind, a, b);
}

class Parser{
    string s;
    size_t p = 0;

    void skip(){
	while(p < s.size() && isspace(static_cast<unsigned char>(s[p]))) 
	++p;
	}
    bool eat(char c){ 
	skip(); 
	if(p < s.size() && s[p] == c){ 
	++p; 
	return true; 
	}
	return false; 
	}
    bool startsPrimary(){
        skip();
        if(p >= s.size())
		return false;
        char c = s[p];
        return isalpha(static_cast<unsigned char>(c)) || isdigit(static_cast<unsigned char>(c)) ||c == '.' || c == '(';
        
    }

    string identifier(){
        skip();
        size_t start = p;
        while(p < s.size() && isalpha(static_cast<unsigned char>(s[p]))) 
		++p;
        return s.substr(start, p - start);
    }

    Expr expression(){
        Expr e = term();
        while (true) {
            if (eat('+')) 
			e = binary(Kind::Add, e, term());
            else if (eat('-')) 
			e = binary(Kind::Sub, e, term());
            else
			break;
        }
        return e;
    }

    Expr term(){
        Expr e = unaryExpr();
        while (true){
            if (eat('*')) 
			e = binary(Kind::Mul, e, unaryExpr());
            else if (eat('/')) 
			e = binary(Kind::Div, e, unaryExpr());
            else if(startsPrimary()){
            e = binary(Kind::Mul, e, unaryExpr());
            } 
			else
			break;
        }
        return e;
    }

    Expr unaryExpr(){
        if(eat('+')) 
		return unaryExpr();
        if(eat('-')) 
		return unary(Kind::Neg, unaryExpr());
        return power();
    }

    Expr power(){
        Expr e = primary();
        if (eat('^'))
		e = binary(Kind::Pow, e, unaryExpr());
        return e;
    }

    Expr primary(){
        skip();
        if (eat('(')){
            Expr e = expression();
            if (!eat(')')) throw runtime_error("Missing closing parenthesis ')'.");
            return e;
        }

        skip();
        if (p < s.size() && (isdigit(static_cast<unsigned char>(s[p])) || s[p] == '.')){
            size_t start = p;
            bool dot = false;
            while(p < s.size() && (isdigit(static_cast<unsigned char>(s[p])) || s[p] == '.')){
                if(s[p] == '.'){
                    if (dot) throw runtime_error("Invalid number.");
                    dot = true;
                }
                ++p;
            }
            return num(stod(s.substr(start, p - start)));
        }

        if(p < s.size() && isalpha(static_cast<unsigned char>(s[p]))){
            string id = identifier();
            if(id == "x" || id == "y" || id == "z") 
			return var(id);
            if(id == "pi") 
			return num(acos(-1.0));
            if(id == "e") 
			return num(exp(1.0));

            // Treat adjacent variable letters such as xy, xyz, and yx as multiplication.
            bool onlyVariables = !id.empty();
            for (char ch : id) {
                if (ch != 'x' && ch != 'y' && ch != 'z') {
                    onlyVariables = false;
                    break;
                }
            }
            if (onlyVariables) {
                Expr product = var(string(1, id[0]));
                for(size_t i = 1; i < id.size(); ++i)
                    product = binary(Kind::Mul, product, var(string(1, id[i])));
                return product;
            }

            Kind k;
            if (id == "sin") 
			k = Kind::Sin;
            else if (id == "cos") 
			k = Kind::Cos;
            else if (id == "tan") 
			k = Kind::Tan;
            else if (id == "exp") 
			k = Kind::Exp;
            else if (id == "ln") 
			k = Kind::Ln;
            else 
			throw runtime_error("Unknown variable or function: " + id);

            if(!eat('(')) 
			throw runtime_error("Function " + id + " must be followed by parentheses.");
            Expr arg = expression();
            if(!eat(')')) 
			throw runtime_error("Missing ')' after function " + id + ".");
            return unary(k, arg);
        }

        throw runtime_error("Unexpected character near position " + to_string(p + 1) + ".");
    }

public:
    explicit Parser(string input) : s(std::move(input)) {
	
	}
    Expr parse(){
        Expr e = expression();
        skip();
        if (p != s.size()) 
		throw runtime_error("Unexpected input near: " + s.substr(p));
        return simplify(e);
    }
};

Expr derivative(const Expr& e, const string& v){
    if (!e) 
	return num(0);
    switch (e->kind){
        case Kind::Number: 
		return num(0);
        case Kind::Variable: 
		return num(e->name == v ? 1 : 0);
        case Kind::Add:
            return simplify(binary(Kind::Add, derivative(e->left, v), derivative(e->right, v)));
        case Kind::Sub:
            return simplify(binary(Kind::Sub, derivative(e->left, v), derivative(e->right, v)));
        case Kind::Mul:
            return simplify(binary(Kind::Add, binary(Kind::Mul, derivative(e->left, v), e->right), binary(Kind::Mul, e->left, derivative(e->right, v))));
        case Kind::Div:
            return simplify(binary(Kind::Div, binary(Kind::Sub, binary(Kind::Mul, derivative(e->left, v), e->right), binary(Kind::Mul, e->left, derivative(e->right, v))), binary(Kind::Pow, e->right, num(2))));
        case Kind::Pow:{
            Expr a = e->left, b = e->right;
            if (b->kind == Kind::Number){
                return simplify(binary(Kind::Mul, binary(Kind::Mul, b, binary(Kind::Pow, a, num(b->value - 1))), derivative(a, v)));
            }
            // General power rule for positive base: d(a^b)=a^b*(b' ln(a)+b*a'/a)
            Expr inside = binary(Kind::Add, binary(Kind::Mul, derivative(b, v), unary(Kind::Ln, a)), binary(Kind::Mul, b, binary(Kind::Div, derivative(a, v), a)));
            return simplify(binary(Kind::Mul, e, inside));
        }
        case Kind::Neg:
            return simplify(unary(Kind::Neg, derivative(e->left, v)));
        case Kind::Sin:
            return simplify(binary(Kind::Mul, unary(Kind::Cos, e->left), derivative(e->left, v)));
        case Kind::Cos:
            return simplify(binary(Kind::Mul, unary(Kind::Neg, unary(Kind::Sin, e->left)), derivative(e->left, v)));
        case Kind::Tan:
            return simplify(binary(Kind::Mul, binary(Kind::Div, num(1), binary(Kind::Pow, unary(Kind::Cos, e->left), num(2))), derivative(e->left, v)));
        case Kind::Exp:
            return simplify(binary(Kind::Mul, unary(Kind::Exp, e->left), derivative(e->left, v)));
        case Kind::Ln:
            return simplify(binary(Kind::Div, derivative(e->left, v), e->left));
    }
    return num(0);
}

bool validVariable(const string& s){ 
    return s == "x" || s == "y" || s == "z"; }
string askVariable(const string& prompt){
    while(true){
        cout << prompt;
        string v;
        cin >> v;
        if(validVariable(v)) 
		return v;
        cout << "Please enter x, y, or z.\n";
    }
}

int main(){
    string input;
    cout << "Enter function: ";
    getline(cin, input);
    Expr function;
    try{
        function = Parser(input).parse();
        cout << "\nFunction accepted: " << printExpr(function) << '\n';
    } 
	catch(const exception& e){
        cout << "Input error: " << e.what() << '\n';
        return 1;
    }
    cout << "\n========== FIRST PARTIAL DERIVATIVES ==========\n";
    try{
        cout << "df/dx = " << printExpr(simplify(derivative(function, "x"))) << '\n';
        cout << "df/dy = " << printExpr(simplify(derivative(function, "y"))) << '\n';
        cout << "df/dz = " << printExpr(simplify(derivative(function, "z"))) << '\n';
    } 
	catch(const exception& e){
        cout << "Calculation error: " << e.what() << '\n';
        return 1;
    }

    return 0;
}
#include<iostream>
#include<string>
#include<memory>
#include<stdexcept>
#include<cctype>
#include<cmath>
#include<sstream>
#include<iomanip>
#include<vector>
#include<algorithm>
using namespace std;
enum class Kind{ 
            Number, 
            Variable, 
			Add, 
			Sub, 
			Mul, 
			Div, 
			Pow, 
			Neg, 
			Sin, 
			Cos, 
			Tan, 
			Exp, 
			Ln 
	    };
struct Node{
    Kind kind;
    double value;
    string name;
    shared_ptr<Node> left, right;

    Node(Kind k, double v = 0.0, string n = "", shared_ptr<Node> l = nullptr, shared_ptr<Node> r = nullptr): kind(k), value(v), name(std::move(n)), left(std::move(l)), right(std::move(r)) {
	
	}
};
using Expr = shared_ptr<Node>;

Expr num(double v){ 
    return make_shared<Node>(Kind::Number, v); 
	}
Expr var(const string& s){ 
    return make_shared<Node>(Kind::Variable, 0, s); 
	}
Expr unary(Kind k, Expr a){ 
    return make_shared<Node>(k, 0, "", a);
	}
Expr binary(Kind k, Expr a, Expr b){ 
    return make_shared<Node>(k, 0, "", a, b);
	}

bool isNum(const Expr& e, double v){
    return e && e->kind == Kind::Number && fabs(e->value - v) < 1e-12;
}

string numberText(double v){
    if (fabs(v) < 1e-12)
	v = 0;
    ostringstream out;
    out << setprecision(12) << v;
    return out.str();
}

string printExpr(const Expr& e){
    if (!e) return "?";
    switch (e->kind){
        case Kind::Number: 
		return numberText(e->value);
        case Kind::Variable: 
		return e->name;
        case Kind::Add: 
		return "(" + printExpr(e->left) + "+" + printExpr(e->right) + ")";
        case Kind::Sub: 
		return "(" + printExpr(e->left) + "-" + printExpr(e->right) + ")";
        case Kind::Mul: 
		return "(" + printExpr(e->left) + "*" + printExpr(e->right) + ")";
        case Kind::Div: 
		return "(" + printExpr(e->left) + "/" + printExpr(e->right) + ")";
        case Kind::Pow: 
		return "(" + printExpr(e->left) + "^" + printExpr(e->right) + ")";
        case Kind::Neg: 
		return "(-" + printExpr(e->left) + ")";
        case Kind::Sin: 
		return "sin(" + printExpr(e->left) + ")";
        case Kind::Cos: 
		return "cos(" + printExpr(e->left) + ")";
        case Kind::Tan: 
		return "tan(" + printExpr(e->left) + ")";
        case Kind::Exp: 
		return "exp(" + printExpr(e->left) + ")";
        case Kind::Ln:  
		return "ln(" + printExpr(e->left) + ")";
    }
    return "?";
}

Expr simplify(const Expr& e) {
    if (!e) 
	return e;
    if (e->kind == Kind::Number || e->kind == Kind::Variable) 
	return e;

    Expr a = simplify(e->left);
    Expr b = e->right ? simplify(e->right) : nullptr;

    if(e->kind == Kind::Neg){
        if (isNum(a, 0)) 
		return num(0);
        if (a->kind == Kind::Number) 
		return num(-a->value);
        if (a->kind == Kind::Neg) 
		return a->left;
        return unary(Kind::Neg, a);
    }

    if(e->kind == Kind::Sin || e->kind == Kind::Cos || e->kind == Kind::Tan || e->kind == Kind::Exp || e->kind == Kind::Ln){
        if(a->kind == Kind::Number){
            double x = a->value;
            if (e->kind == Kind::Sin) 
			return num(sin(x));
            if (e->kind == Kind::Cos) 
			return num(cos(x));
            if (e->kind == Kind::Tan) 
			return num(tan(x));
            if (e->kind == Kind::Exp) 
			return num(exp(x));
            if (e->kind == Kind::Ln && x > 0) 
			return num(log(x));
        }
        return unary(e->kind, a);
    }

    if(a->kind == Kind::Number && b->kind == Kind::Number){
        double x = a->value, y = b->value;
          switch (e->kind) {
            case Kind::Add: 
			return num(x + y);
            case Kind::Sub: 
			return num(x - y);
            case Kind::Mul: 
			return num(x * y);
            case Kind::Div: 
			if (fabs(y) > 1e-12) 
			return num(x / y); 
			break;
            case Kind::Pow:
            if (x >= 0 || fabs(y - round(y)) < 1e-12) 
			return num(pow(x, y));
            break;
            default: 
			break;
        }
    }

    switch (e->kind){
        case Kind::Add:
            if(isNum(a, 0)) 
			return b;
            if(isNum(b, 0)) 
			return a;
            break;
        case Kind::Sub:
            if (isNum(b, 0)) 
			return a;
            if (isNum(a, 0)) 
			return unary(Kind::Neg, b);
            if (printExpr(a) == printExpr(b)) 
			return num(0);
            break;
        case Kind::Mul:
            if(isNum(a, 0) || isNum(b, 0)) 
			return num(0);
            if(isNum(a, 1)) 
			return b;
            if(isNum(b, 1)) 
			return a;
            if(isNum(a, -1)) 
			return unary(Kind::Neg, b);
            if(isNum(b, -1)) 
			return unary(Kind::Neg, a);
            break;
        case Kind::Div:
            if (isNum(a, 0) && !isNum(b, 0)) 
			return num(0);
            if (isNum(b, 1)) 
			return a;
            if (printExpr(a) == printExpr(b) && a->kind != Kind::Number) 
			return num(1);
            break;
        case Kind::Pow:
            if(isNum(b, 0)) 
			return num(1);
            if(isNum(b, 1)) 
			return a;
            if(isNum(a, 1)) 
			return num(1);
            break;
        default: 
		     break;
    }
    return binary(e->kind, a, b);
}

class Parser{
    string s;
    size_t p = 0;

    void skip(){
	while(p < s.size() && isspace(static_cast<unsigned char>(s[p]))) 
	++p;
	}
    bool eat(char c){ 
	skip(); 
	if(p < s.size() && s[p] == c){ 
	++p; 
	return true; 
	}
	return false; 
	}
    bool startsPrimary(){
        skip();
        if(p >= s.size())
		return false;
        char c = s[p];
        return isalpha(static_cast<unsigned char>(c)) || isdigit(static_cast<unsigned char>(c)) ||c == '.' || c == '(';
        
    }

    string identifier(){
        skip();
        size_t start = p;
        while(p < s.size() && isalpha(static_cast<unsigned char>(s[p]))) 
		++p;
        return s.substr(start, p - start);
    }

    Expr expression(){
        Expr e = term();
        while (true) {
            if (eat('+')) 
			e = binary(Kind::Add, e, term());
            else if (eat('-')) 
			e = binary(Kind::Sub, e, term());
            else
			break;
        }
        return e;
    }

    Expr term(){
        Expr e = unaryExpr();
        while (true){
            if (eat('*')) 
			e = binary(Kind::Mul, e, unaryExpr());
            else if (eat('/')) 
			e = binary(Kind::Div, e, unaryExpr());
            else if(startsPrimary()){
            e = binary(Kind::Mul, e, unaryExpr());
            } 
			else
			break;
        }
        return e;
    }

    Expr unaryExpr(){
        if(eat('+')) 
		return unaryExpr();
        if(eat('-')) 
		return unary(Kind::Neg, unaryExpr());
        return power();
    }

    Expr power(){
        Expr e = primary();
        if (eat('^'))
		e = binary(Kind::Pow, e, unaryExpr());
        return e;
    }

    Expr primary(){
        skip();
        if (eat('(')){
            Expr e = expression();
            if (!eat(')')) throw runtime_error("Missing closing parenthesis ')'.");
            return e;
        }

        skip();
        if (p < s.size() && (isdigit(static_cast<unsigned char>(s[p])) || s[p] == '.')){
            size_t start = p;
            bool dot = false;
            while(p < s.size() && (isdigit(static_cast<unsigned char>(s[p])) || s[p] == '.')){
                if(s[p] == '.'){
                    if (dot) throw runtime_error("Invalid number.");
                    dot = true;
                }
                ++p;
            }
            return num(stod(s.substr(start, p - start)));
        }

        if(p < s.size() && isalpha(static_cast<unsigned char>(s[p]))){
            string id = identifier();
            if(id == "x" || id == "y" || id == "z") 
			return var(id);
            if(id == "pi") 
			return num(acos(-1.0));
            if(id == "e") 
			return num(exp(1.0));

            // Treat adjacent variable letters such as xy, xyz, and yx as multiplication.
            bool onlyVariables = !id.empty();
            for (char ch : id) {
                if (ch != 'x' && ch != 'y' && ch != 'z') {
                    onlyVariables = false;
                    break;
                }
            }
            if (onlyVariables) {
                Expr product = var(string(1, id[0]));
                for(size_t i = 1; i < id.size(); ++i)
                    product = binary(Kind::Mul, product, var(string(1, id[i])));
                return product;
            }

            Kind k;
            if (id == "sin") 
			k = Kind::Sin;
            else if (id == "cos") 
			k = Kind::Cos;
            else if (id == "tan") 
			k = Kind::Tan;
            else if (id == "exp") 
			k = Kind::Exp;
            else if (id == "ln") 
			k = Kind::Ln;
            else 
			throw runtime_error("Unknown variable or function: " + id);

            if(!eat('(')) 
			throw runtime_error("Function " + id + " must be followed by parentheses.");
            Expr arg = expression();
            if(!eat(')')) 
			throw runtime_error("Missing ')' after function " + id + ".");
            return unary(k, arg);
        }

        throw runtime_error("Unexpected character near position " + to_string(p + 1) + ".");
    }

public:
    explicit Parser(string input) : s(std::move(input)) {
	
	}
    Expr parse(){
        Expr e = expression();
        skip();
        if (p != s.size()) 
		throw runtime_error("Unexpected input near: " + s.substr(p));
        return simplify(e);
    }
};

Expr derivative(const Expr& e, const string& v){
    if (!e) 
	return num(0);
    switch (e->kind){
        case Kind::Number: 
		return num(0);
        case Kind::Variable: 
		return num(e->name == v ? 1 : 0);
        case Kind::Add:
            return simplify(binary(Kind::Add, derivative(e->left, v), derivative(e->right, v)));
        case Kind::Sub:
            return simplify(binary(Kind::Sub, derivative(e->left, v), derivative(e->right, v)));
        case Kind::Mul:
            return simplify(binary(Kind::Add, binary(Kind::Mul, derivative(e->left, v), e->right), binary(Kind::Mul, e->left, derivative(e->right, v))));
        case Kind::Div:
            return simplify(binary(Kind::Div, binary(Kind::Sub, binary(Kind::Mul, derivative(e->left, v), e->right), binary(Kind::Mul, e->left, derivative(e->right, v))), binary(Kind::Pow, e->right, num(2))));
        case Kind::Pow:{
            Expr a = e->left, b = e->right;
            if (b->kind == Kind::Number){
                return simplify(binary(Kind::Mul, binary(Kind::Mul, b, binary(Kind::Pow, a, num(b->value - 1))), derivative(a, v)));
            }
            // General power rule for positive base: d(a^b)=a^b*(b' ln(a)+b*a'/a)
            Expr inside = binary(Kind::Add, binary(Kind::Mul, derivative(b, v), unary(Kind::Ln, a)), binary(Kind::Mul, b, binary(Kind::Div, derivative(a, v), a)));
            return simplify(binary(Kind::Mul, e, inside));
        }
        case Kind::Neg:
            return simplify(unary(Kind::Neg, derivative(e->left, v)));
        case Kind::Sin:
            return simplify(binary(Kind::Mul, unary(Kind::Cos, e->left), derivative(e->left, v)));
        case Kind::Cos:
            return simplify(binary(Kind::Mul, unary(Kind::Neg, unary(Kind::Sin, e->left)), derivative(e->left, v)));
        case Kind::Tan:
            return simplify(binary(Kind::Mul, binary(Kind::Div, num(1), binary(Kind::Pow, unary(Kind::Cos, e->left), num(2))), derivative(e->left, v)));
        case Kind::Exp:
            return simplify(binary(Kind::Mul, unary(Kind::Exp, e->left), derivative(e->left, v)));
        case Kind::Ln:
            return simplify(binary(Kind::Div, derivative(e->left, v), e->left));
    }
    return num(0);
}

bool validVariable(const string& s){ 
    return s == "x" || s == "y" || s == "z"; }
string askVariable(const string& prompt){
    while(true){
        cout << prompt;
        string v;
        cin >> v;
        if(validVariable(v)) 
		return v;
        cout << "Please enter x, y, or z.\n";
    }
}

int main(){
    string input;
    cout << "Enter function: ";
    getline(cin, input);
    Expr function;
    try{
        function = Parser(input).parse();
        cout << "\nFunction accepted: " << printExpr(function) << '\n';
    } 
	catch(const exception& e){
        cout << "Input error: " << e.what() << '\n';
        return 1;
    }
    cout << "\n========== FIRST PARTIAL DERIVATIVES ==========\n";
    try{
        cout << "df/dx = " << printExpr(simplify(derivative(function, "x"))) << '\n';
        cout << "df/dy = " << printExpr(simplify(derivative(function, "y"))) << '\n';
        cout << "df/dz = " << printExpr(simplify(derivative(function, "z"))) << '\n';
    } 
	catch(const exception& e){
        cout << "Calculation error: " << e.what() << '\n';
        return 1;
    }

    return 0;
}
