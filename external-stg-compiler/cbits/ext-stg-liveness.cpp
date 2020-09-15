
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_1__0__1 {
using t_tuple = Tuple<RamDomain, 1>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (a[0] < b[0]) ? -1 : ((a[0] > b[0]) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return  a[0] < b[0];
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return a[0] == b[0];
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[1];
std::copy(ramDomain, ramDomain + 1, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0) {
RamDomain data[1] = {a0};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_0(const t_tuple& lower, const t_tuple& upper, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_0(const t_tuple& lower, const t_tuple& upper) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper, context& h) const {
auto pos = ind_0.find(lower, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_1(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 1 direct b-tree index 0 lex-order [0]\n";
ind_0.printStats(o);
}
};
struct t_btree_2__0_1__01__11 {
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (a[0] < b[0]) ? -1 : ((a[0] > b[0]) ? 1 :((a[1] < b[1]) ? -1 : ((a[1] > b[1]) ? 1 :(0))));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return  a[0] < b[0]|| (a[0] == b[0] && ( a[1] < b[1]));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return a[0] == b[0]&&a[1] == b[1];
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_00(const t_tuple& lower, const t_tuple& upper, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& lower, const t_tuple& upper) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_01(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_tuple low(lower); t_tuple high(lower);
low[1] = MIN_RAM_SIGNED;
high[1] = MAX_RAM_SIGNED;
return make_range(ind_0.lower_bound(low, h.hints_0), ind_0.upper_bound(high, h.hints_0));
}
range<t_ind_0::iterator> lowerUpperRange_01(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_01(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
auto pos = ind_0.find(lower, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 2 direct b-tree index 0 lex-order [0,1]\n";
ind_0.printStats(o);
}
};
struct t_btree_2__1_0__10__11 {
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (a[1] < b[1]) ? -1 : ((a[1] > b[1]) ? 1 :((a[0] < b[0]) ? -1 : ((a[0] > b[0]) ? 1 :(0))));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return  a[1] < b[1]|| (a[1] == b[1] && ( a[0] < b[0]));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return a[1] == b[1]&&a[0] == b[0];
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_00(const t_tuple& lower, const t_tuple& upper, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& lower, const t_tuple& upper) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_tuple low(lower); t_tuple high(lower);
low[0] = MIN_RAM_SIGNED;
high[0] = MAX_RAM_SIGNED;
return make_range(ind_0.lower_bound(low, h.hints_0), ind_0.upper_bound(high, h.hints_0));
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_10(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
auto pos = ind_0.find(lower, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 2 direct b-tree index 0 lex-order [1,0]\n";
ind_0.printStats(o);
}
};

class Sf_ext_stg_liveness : public SouffleProgram {
private:
static inline bool regex_wrapper(const std::string& pattern, const std::string& text) {
   bool result = false; 
   try { result = std::regex_match(text, std::regex(pattern)); } catch(...) { 
     std::cerr << "warning: wrong pattern provided for match(\"" << pattern << "\",\"" << text << "\").\n";
}
   return result;
}
private:
static inline std::string substr_wrapper(const std::string& str, size_t idx, size_t len) {
   std::string result; 
   try { result = str.substr(idx,len); } catch(...) { 
     std::cerr << "warning: wrong index position provided by substr(\"";
     std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
   } return result;
}
public:
// -- initialize symbol table --
SymbolTable symTable;// -- initialize record table --
RecordTable recordTable;
// -- Table: @delta_LiveFunName
std::unique_ptr<t_btree_1__0__1> rel_1_delta_LiveFunName = std::make_unique<t_btree_1__0__1>();
// -- Table: @new_LiveFunName
std::unique_ptr<t_btree_1__0__1> rel_2_new_LiveFunName = std::make_unique<t_btree_1__0__1>();
// -- Table: DataConReference
std::unique_ptr<t_btree_2__0_1__01__11> rel_3_DataConReference = std::make_unique<t_btree_2__0_1__01__11>();
souffle::RelationWrapper<0,t_btree_2__0_1__01__11,Tuple<RamDomain,2>,2,0> wrapper_rel_3_DataConReference;
// -- Table: FunReference
std::unique_ptr<t_btree_2__0_1__01__11> rel_4_FunReference = std::make_unique<t_btree_2__0_1__01__11>();
souffle::RelationWrapper<1,t_btree_2__0_1__01__11,Tuple<RamDomain,2>,2,0> wrapper_rel_4_FunReference;
// -- Table: LiveDataConName
std::unique_ptr<t_btree_1__0__1> rel_5_LiveDataConName = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<2,t_btree_1__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_5_LiveDataConName;
// -- Table: LiveFunName
std::unique_ptr<t_btree_1__0__1> rel_6_LiveFunName = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<3,t_btree_1__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_6_LiveFunName;
// -- Table: LiveSource
std::unique_ptr<t_btree_1__0__1> rel_7_LiveSource = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<4,t_btree_1__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_7_LiveSource;
// -- Table: LiveTyConName
std::unique_ptr<t_btree_1__0__1> rel_8_LiveTyConName = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<5,t_btree_1__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_8_LiveTyConName;
// -- Table: TyCon
std::unique_ptr<t_btree_2__1_0__10__11> rel_9_TyCon = std::make_unique<t_btree_2__1_0__10__11>();
souffle::RelationWrapper<6,t_btree_2__1_0__10__11,Tuple<RamDomain,2>,2,0> wrapper_rel_9_TyCon;
// -- Table: TyConReference
std::unique_ptr<t_btree_2__0_1__01__11> rel_10_TyConReference = std::make_unique<t_btree_2__0_1__01__11>();
souffle::RelationWrapper<7,t_btree_2__0_1__01__11,Tuple<RamDomain,2>,2,0> wrapper_rel_10_TyConReference;
public:
Sf_ext_stg_liveness() : 
wrapper_rel_3_DataConReference(*rel_3_DataConReference,symTable,"DataConReference",std::array<const char *,2>{{"s:Name","s:Name"}},std::array<const char *,2>{{"fun","datacon"}}),

wrapper_rel_4_FunReference(*rel_4_FunReference,symTable,"FunReference",std::array<const char *,2>{{"s:Name","s:Name"}},std::array<const char *,2>{{"fun","funref"}}),

wrapper_rel_5_LiveDataConName(*rel_5_LiveDataConName,symTable,"LiveDataConName",std::array<const char *,1>{{"s:Name"}},std::array<const char *,1>{{"datacon"}}),

wrapper_rel_6_LiveFunName(*rel_6_LiveFunName,symTable,"LiveFunName",std::array<const char *,1>{{"s:Name"}},std::array<const char *,1>{{"fun"}}),

wrapper_rel_7_LiveSource(*rel_7_LiveSource,symTable,"LiveSource",std::array<const char *,1>{{"s:Name"}},std::array<const char *,1>{{"fun"}}),

wrapper_rel_8_LiveTyConName(*rel_8_LiveTyConName,symTable,"LiveTyConName",std::array<const char *,1>{{"s:Name"}},std::array<const char *,1>{{"tycon"}}),

wrapper_rel_9_TyCon(*rel_9_TyCon,symTable,"TyCon",std::array<const char *,2>{{"s:Name","s:Name"}},std::array<const char *,2>{{"tycon","datacon"}}),

wrapper_rel_10_TyConReference(*rel_10_TyConReference,symTable,"TyConReference",std::array<const char *,2>{{"s:Name","s:Name"}},std::array<const char *,2>{{"fun","tycon"}}){
addRelation("DataConReference",&wrapper_rel_3_DataConReference,true,false);
addRelation("FunReference",&wrapper_rel_4_FunReference,true,false);
addRelation("LiveDataConName",&wrapper_rel_5_LiveDataConName,false,true);
addRelation("LiveFunName",&wrapper_rel_6_LiveFunName,false,true);
addRelation("LiveSource",&wrapper_rel_7_LiveSource,true,false);
addRelation("LiveTyConName",&wrapper_rel_8_LiveTyConName,false,true);
addRelation("TyCon",&wrapper_rel_9_TyCon,true,false);
addRelation("TyConReference",&wrapper_rel_10_TyConReference,true,false);
}
~Sf_ext_stg_liveness() {
}
private:
std::string inputDirectory;
std::string outputDirectory;
bool performIO;
std::atomic<RamDomain> ctr{};

std::atomic<size_t> iter{};
void runFunction(std::string inputDirectory = "", std::string outputDirectory = "", bool performIO = false) {
this->inputDirectory = inputDirectory;
this->outputDirectory = outputDirectory;
this->performIO = performIO;
SignalHandler::instance()->set();
#if defined(_OPENMP)
if (getNumThreads() > 0) {omp_set_num_threads(getNumThreads());}
#endif

// -- query evaluation --
{
 std::vector<RamDomain> args, ret;
subroutine_0(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_1(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_2(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_3(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_4(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_5(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_6(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_7(args, ret);
}

// -- relation hint statistics --
SignalHandler::instance()->reset();
}
public:
void run() override { runFunction("", "", false); }
public:
void runAll(std::string inputDirectory = "", std::string outputDirectory = "") override { runFunction(inputDirectory, outputDirectory, true);
}
public:
void printAll(std::string outputDirectory = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","datacon"},{"name","LiveDataConName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"datacon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_5_LiveDataConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun"},{"name","LiveFunName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"fun\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_6_LiveFunName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","tycon"},{"name","LiveTyConName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"tycon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_8_LiveTyConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectory = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun"},{"fact-dir","."},{"name","LiveSource"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"fun\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_7_LiveSource);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\tdatacon"},{"fact-dir","."},{"name","DataConReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"fun\", \"datacon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_DataConReference);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\tfunref"},{"fact-dir","."},{"name","FunReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"fun\", \"funref\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_4_FunReference);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\ttycon"},{"fact-dir","."},{"name","TyConReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"fun\", \"tycon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_10_TyConReference);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","tycon\tdatacon"},{"fact-dir","."},{"name","TyCon"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"tycon\", \"datacon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_9_TyCon);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
public:
void dumpInputs(std::ostream& out = std::cout) override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "LiveSource";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_7_LiveSource);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "DataConReference";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_3_DataConReference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "FunReference";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_FunReference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "TyConReference";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_10_TyConReference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "TyCon";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_9_TyCon);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs(std::ostream& out = std::cout) override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "LiveDataConName";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_5_LiveDataConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "LiveFunName";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_6_LiveFunName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "LiveTyConName";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_8_LiveTyConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
void executeSubroutine(std::string name, const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) override {
if (name == "stratum_0") {
subroutine_0(args, ret);
return;}
if (name == "stratum_1") {
subroutine_1(args, ret);
return;}
if (name == "stratum_2") {
subroutine_2(args, ret);
return;}
if (name == "stratum_3") {
subroutine_3(args, ret);
return;}
if (name == "stratum_4") {
subroutine_4(args, ret);
return;}
if (name == "stratum_5") {
subroutine_5(args, ret);
return;}
if (name == "stratum_6") {
subroutine_6(args, ret);
return;}
if (name == "stratum_7") {
subroutine_7(args, ret);
return;}
fatal("unknown subroutine");
}
void subroutine_0(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","tycon\tdatacon"},{"fact-dir","."},{"name","TyCon"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"tycon\", \"datacon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_9_TyCon);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
void subroutine_1(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\ttycon"},{"fact-dir","."},{"name","TyConReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"fun\", \"tycon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_10_TyConReference);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
void subroutine_2(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\tdatacon"},{"fact-dir","."},{"name","DataConReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"fun\", \"datacon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_DataConReference);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
void subroutine_3(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\tfunref"},{"fact-dir","."},{"name","FunReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"fun\", \"funref\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_4_FunReference);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
void subroutine_4(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun"},{"fact-dir","."},{"name","LiveSource"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"fun\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_7_LiveSource);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}
void subroutine_5(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
SignalHandler::instance()->setMsg(R"_(LiveFunName(fun) :- 
   LiveSource(fun).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [32:1-33:19])_");
if(!(rel_7_LiveSource->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_7_LiveSource_op_ctxt,rel_7_LiveSource->createContext());
CREATE_OP_CONTEXT(rel_6_LiveFunName_op_ctxt,rel_6_LiveFunName->createContext());
for(const auto& env0 : *rel_7_LiveSource) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_6_LiveFunName->insert(tuple,READ_OP_CONTEXT(rel_6_LiveFunName_op_ctxt));
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_6_LiveFunName_op_ctxt,rel_6_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_1_delta_LiveFunName_op_ctxt,rel_1_delta_LiveFunName->createContext());
for(const auto& env0 : *rel_6_LiveFunName) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_1_delta_LiveFunName->insert(tuple,READ_OP_CONTEXT(rel_1_delta_LiveFunName_op_ctxt));
}
}
();iter = 0;
for(;;) {
SignalHandler::instance()->setMsg(R"_(LiveFunName(ref) :- 
   LiveFunName(fun),
   FunReference(fun,ref).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [35:1-37:26])_");
if(!(rel_1_delta_LiveFunName->empty()) && !(rel_4_FunReference->empty())) {
[&](){
auto part = rel_1_delta_LiveFunName->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_2_new_LiveFunName_op_ctxt,rel_2_new_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_6_LiveFunName_op_ctxt,rel_6_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_4_FunReference_op_ctxt,rel_4_FunReference->createContext());
CREATE_OP_CONTEXT(rel_1_delta_LiveFunName_op_ctxt,rel_1_delta_LiveFunName->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
const Tuple<RamDomain,2> lower{{ramBitCast(env0[0]),0}};
const Tuple<RamDomain,2> upper{{ramBitCast(env0[0]),0}};
auto range = rel_4_FunReference->lowerUpperRange_01(lower, upper,READ_OP_CONTEXT(rel_4_FunReference_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_6_LiveFunName->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_6_LiveFunName_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_2_new_LiveFunName->insert(tuple,READ_OP_CONTEXT(rel_2_new_LiveFunName_op_ctxt));
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END
}
();}
if(rel_2_new_LiveFunName->empty()) break;
[&](){
CREATE_OP_CONTEXT(rel_2_new_LiveFunName_op_ctxt,rel_2_new_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_6_LiveFunName_op_ctxt,rel_6_LiveFunName->createContext());
for(const auto& env0 : *rel_2_new_LiveFunName) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_6_LiveFunName->insert(tuple,READ_OP_CONTEXT(rel_6_LiveFunName_op_ctxt));
}
}
();std::swap(rel_1_delta_LiveFunName, rel_2_new_LiveFunName);
rel_2_new_LiveFunName->purge();
iter++;
}
iter = 0;
rel_1_delta_LiveFunName->purge();
rel_2_new_LiveFunName->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun"},{"name","LiveFunName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"fun\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_6_LiveFunName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) rel_4_FunReference->purge();
}
void subroutine_6(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
SignalHandler::instance()->setMsg(R"_(LiveDataConName(fun) :- 
   LiveSource(fun).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [40:1-41:19])_");
if(!(rel_7_LiveSource->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_7_LiveSource_op_ctxt,rel_7_LiveSource->createContext());
CREATE_OP_CONTEXT(rel_5_LiveDataConName_op_ctxt,rel_5_LiveDataConName->createContext());
for(const auto& env0 : *rel_7_LiveSource) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_5_LiveDataConName->insert(tuple,READ_OP_CONTEXT(rel_5_LiveDataConName_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(LiveDataConName(datacon) :- 
   LiveFunName(fun),
   DataConReference(fun,datacon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [43:1-45:34])_");
if(!(rel_6_LiveFunName->empty()) && !(rel_3_DataConReference->empty())) {
[&](){
auto part = rel_6_LiveFunName->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_5_LiveDataConName_op_ctxt,rel_5_LiveDataConName->createContext());
CREATE_OP_CONTEXT(rel_6_LiveFunName_op_ctxt,rel_6_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_3_DataConReference_op_ctxt,rel_3_DataConReference->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
const Tuple<RamDomain,2> lower{{ramBitCast(env0[0]),0}};
const Tuple<RamDomain,2> upper{{ramBitCast(env0[0]),0}};
auto range = rel_3_DataConReference->lowerUpperRange_01(lower, upper,READ_OP_CONTEXT(rel_3_DataConReference_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_5_LiveDataConName->insert(tuple,READ_OP_CONTEXT(rel_5_LiveDataConName_op_ctxt));
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END
}
();}
SignalHandler::instance()->setMsg(R"_(LiveDataConName(datacon) :- 
   TyCon(_,datacon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [59:1-60:21])_");
if(!(rel_9_TyCon->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_5_LiveDataConName_op_ctxt,rel_5_LiveDataConName->createContext());
CREATE_OP_CONTEXT(rel_9_TyCon_op_ctxt,rel_9_TyCon->createContext());
for(const auto& env0 : *rel_9_TyCon) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[1])}};
rel_5_LiveDataConName->insert(tuple,READ_OP_CONTEXT(rel_5_LiveDataConName_op_ctxt));
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","datacon"},{"name","LiveDataConName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"datacon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_5_LiveDataConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) rel_3_DataConReference->purge();
if (performIO) rel_7_LiveSource->purge();
}
void subroutine_7(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
SignalHandler::instance()->setMsg(R"_(LiveTyConName(tycon) :- 
   LiveDataConName(datacon),
   TyCon(tycon,datacon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [48:1-50:25])_");
if(!(rel_5_LiveDataConName->empty()) && !(rel_9_TyCon->empty())) {
[&](){
auto part = rel_5_LiveDataConName->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_8_LiveTyConName_op_ctxt,rel_8_LiveTyConName->createContext());
CREATE_OP_CONTEXT(rel_5_LiveDataConName_op_ctxt,rel_5_LiveDataConName->createContext());
CREATE_OP_CONTEXT(rel_9_TyCon_op_ctxt,rel_9_TyCon->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
const Tuple<RamDomain,2> lower{{0,ramBitCast(env0[0])}};
const Tuple<RamDomain,2> upper{{0,ramBitCast(env0[0])}};
auto range = rel_9_TyCon->lowerUpperRange_10(lower, upper,READ_OP_CONTEXT(rel_9_TyCon_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[0])}};
rel_8_LiveTyConName->insert(tuple,READ_OP_CONTEXT(rel_8_LiveTyConName_op_ctxt));
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END
}
();}
SignalHandler::instance()->setMsg(R"_(LiveTyConName(tycon) :- 
   LiveFunName(fun),
   TyConReference(fun,tycon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [52:1-54:30])_");
if(!(rel_6_LiveFunName->empty()) && !(rel_10_TyConReference->empty())) {
[&](){
auto part = rel_6_LiveFunName->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_8_LiveTyConName_op_ctxt,rel_8_LiveTyConName->createContext());
CREATE_OP_CONTEXT(rel_6_LiveFunName_op_ctxt,rel_6_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_10_TyConReference_op_ctxt,rel_10_TyConReference->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
const Tuple<RamDomain,2> lower{{ramBitCast(env0[0]),0}};
const Tuple<RamDomain,2> upper{{ramBitCast(env0[0]),0}};
auto range = rel_10_TyConReference->lowerUpperRange_01(lower, upper,READ_OP_CONTEXT(rel_10_TyConReference_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_8_LiveTyConName->insert(tuple,READ_OP_CONTEXT(rel_8_LiveTyConName_op_ctxt));
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","tycon"},{"name","LiveTyConName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"tycon\"]}}"},{"types","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_8_LiveTyConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) rel_10_TyConReference->purge();
if (performIO) rel_9_TyCon->purge();
if (performIO) rel_6_LiveFunName->purge();
if (performIO) rel_5_LiveDataConName->purge();
}
};
SouffleProgram *newInstance_ext_stg_liveness(){return new Sf_ext_stg_liveness;}
SymbolTable *getST_ext_stg_liveness(SouffleProgram *p){return &reinterpret_cast<Sf_ext_stg_liveness*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_ext_stg_liveness: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_ext_stg_liveness();
};
public:
factory_Sf_ext_stg_liveness() : ProgramFactory("ext_stg_liveness"){}
};
extern "C" {
factory_Sf_ext_stg_liveness __factory_Sf_ext_stg_liveness_instance;
}
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(ext-stg-liveness.dl)",
R"()",
R"()",
false,
R"()",
4);
if (!opt.parse(argc,argv)) return 1;
souffle::Sf_ext_stg_liveness obj;
#if defined(_OPENMP) 
obj.setNumThreads(opt.getNumJobs());

#endif
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
