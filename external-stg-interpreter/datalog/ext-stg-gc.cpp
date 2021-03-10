
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_i__0__1 {
using t_tuple = Tuple<RamDomain, 1>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :(0);
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
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
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
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
struct t_btree_ii__0_1__11 {
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
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
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
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
struct t_btree_ii__0_1__11__10 {
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
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
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_10(lower,upper,h);
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

class Sf_ext_stg_gc : public SouffleProgram {
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
// -- Table: @delta_Live
Own<t_btree_i__0__1> rel_1_delta_Live = mk<t_btree_i__0__1>();
// -- Table: @new_Live
Own<t_btree_i__0__1> rel_2_new_Live = mk<t_btree_i__0__1>();
// -- Table: All
Own<t_btree_i__0__1> rel_3_All = mk<t_btree_i__0__1>();
souffle::RelationWrapper<0,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_3_All;
// -- Table: Dead
Own<t_btree_i__0__1> rel_4_Dead = mk<t_btree_i__0__1>();
souffle::RelationWrapper<1,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_4_Dead;
// -- Table: GCRoot
Own<t_btree_i__0__1> rel_5_GCRoot = mk<t_btree_i__0__1>();
souffle::RelationWrapper<2,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_5_GCRoot;
// -- Table: Live
Own<t_btree_i__0__1> rel_6_Live = mk<t_btree_i__0__1>();
souffle::RelationWrapper<3,t_btree_i__0__1,Tuple<RamDomain,1>,1,0> wrapper_rel_6_Live;
// -- Table: LiveReferredBy
Own<t_btree_ii__0_1__11> rel_7_LiveReferredBy = mk<t_btree_ii__0_1__11>();
souffle::RelationWrapper<4,t_btree_ii__0_1__11,Tuple<RamDomain,2>,2,0> wrapper_rel_7_LiveReferredBy;
// -- Table: Reference
Own<t_btree_ii__0_1__11__10> rel_8_Reference = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<5,t_btree_ii__0_1__11__10,Tuple<RamDomain,2>,2,0> wrapper_rel_8_Reference;
public:
Sf_ext_stg_gc() : 
wrapper_rel_3_All(*rel_3_All,symTable,"All",std::array<const char *,1>{{"i:number"}},std::array<const char *,1>{{"val"}}),

wrapper_rel_4_Dead(*rel_4_Dead,symTable,"Dead",std::array<const char *,1>{{"i:number"}},std::array<const char *,1>{{"val"}}),

wrapper_rel_5_GCRoot(*rel_5_GCRoot,symTable,"GCRoot",std::array<const char *,1>{{"i:number"}},std::array<const char *,1>{{"val"}}),

wrapper_rel_6_Live(*rel_6_Live,symTable,"Live",std::array<const char *,1>{{"i:number"}},std::array<const char *,1>{{"val"}}),

wrapper_rel_7_LiveReferredBy(*rel_7_LiveReferredBy,symTable,"LiveReferredBy",std::array<const char *,2>{{"i:number","i:number"}},std::array<const char *,2>{{"to","from"}}),

wrapper_rel_8_Reference(*rel_8_Reference,symTable,"Reference",std::array<const char *,2>{{"i:number","i:number"}},std::array<const char *,2>{{"from","to"}}){
addRelation("All",&wrapper_rel_3_All,false,false);
addRelation("Dead",&wrapper_rel_4_Dead,false,true);
addRelation("GCRoot",&wrapper_rel_5_GCRoot,true,true);
addRelation("Live",&wrapper_rel_6_Live,false,true);
addRelation("LiveReferredBy",&wrapper_rel_7_LiveReferredBy,false,true);
addRelation("Reference",&wrapper_rel_8_Reference,true,true);
}
~Sf_ext_stg_gc() {
}
private:
std::string inputDirectory;
std::string outputDirectory;
bool performIO;
std::atomic<RamDomain> ctr{};

std::atomic<size_t> iter{};
void runFunction(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "", bool performIOArg = false) {
this->inputDirectory = inputDirectoryArg;
this->outputDirectory = outputDirectoryArg;
this->performIO = performIOArg;
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

// -- relation hint statistics --
SignalHandler::instance()->reset();
}
public:
void run() override { runFunction("", "", false); }
public:
void runAll(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "") override { runFunction(inputDirectoryArg, outputDirectoryArg, true);
}
public:
void printAll(std::string outputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","to\tfrom"},{"name","LiveReferredBy"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"to\", \"from\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"i:number\", \"i:number\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_7_LiveReferredBy);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"name","Live"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_6_Live);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"name","Dead"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_Dead);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","from\tto"},{"name","Reference"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"from\", \"to\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"i:number\", \"i:number\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_8_Reference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"name","GCRoot"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_5_GCRoot);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"fact-dir","."},{"name","GCRoot"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_5_GCRoot);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","from\tto"},{"fact-dir","."},{"name","Reference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"from\", \"to\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"i:number\", \"i:number\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_8_Reference);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
public:
void dumpInputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "GCRoot";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_5_GCRoot);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "Reference";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"i:number\", \"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_8_Reference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "LiveReferredBy";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"i:number\", \"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_7_LiveReferredBy);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "Live";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_6_Live);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "Dead";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_Dead);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "Reference";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"i:number\", \"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_8_Reference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "GCRoot";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_5_GCRoot);
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
fatal("unknown subroutine");
}
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_0(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","from\tto"},{"fact-dir","."},{"name","Reference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"from\", \"to\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"i:number\", \"i:number\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_8_Reference);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","from\tto"},{"name","Reference"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"from\", \"to\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"i:number\", \"i:number\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_8_Reference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_1(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"fact-dir","."},{"name","GCRoot"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_5_GCRoot);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"name","GCRoot"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_5_GCRoot);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_2(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
SignalHandler::instance()->setMsg(R"_(Live(ref) :- 
   GCRoot(ref).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-interpreter/datalog/ext-stg-gc.dl [19:1-19:26])_");
if(!(rel_5_GCRoot->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_5_GCRoot_op_ctxt,rel_5_GCRoot->createContext());
CREATE_OP_CONTEXT(rel_6_Live_op_ctxt,rel_6_Live->createContext());
for(const auto& env0 : *rel_5_GCRoot) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_6_Live->insert(tuple,READ_OP_CONTEXT(rel_6_Live_op_ctxt));
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_1_delta_Live_op_ctxt,rel_1_delta_Live->createContext());
CREATE_OP_CONTEXT(rel_6_Live_op_ctxt,rel_6_Live->createContext());
for(const auto& env0 : *rel_6_Live) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_1_delta_Live->insert(tuple,READ_OP_CONTEXT(rel_1_delta_Live_op_ctxt));
}
}
();iter = 0;
for(;;) {
SignalHandler::instance()->setMsg(R"_(Live(to) :- 
   Live(from),
   Reference(from,to).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-interpreter/datalog/ext-stg-gc.dl [21:1-23:23])_");
if(!(rel_1_delta_Live->empty()) && !(rel_8_Reference->empty())) {
[&](){
auto part = rel_1_delta_Live->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_2_new_Live_op_ctxt,rel_2_new_Live->createContext());
CREATE_OP_CONTEXT(rel_1_delta_Live_op_ctxt,rel_1_delta_Live->createContext());
CREATE_OP_CONTEXT(rel_8_Reference_op_ctxt,rel_8_Reference->createContext());
CREATE_OP_CONTEXT(rel_6_Live_op_ctxt,rel_6_Live->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
auto range = rel_8_Reference->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_8_Reference_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_6_Live->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_6_Live_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_2_new_Live->insert(tuple,READ_OP_CONTEXT(rel_2_new_Live_op_ctxt));
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END
}
();}
if(rel_2_new_Live->empty()) break;
[&](){
CREATE_OP_CONTEXT(rel_2_new_Live_op_ctxt,rel_2_new_Live->createContext());
CREATE_OP_CONTEXT(rel_6_Live_op_ctxt,rel_6_Live->createContext());
for(const auto& env0 : *rel_2_new_Live) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_6_Live->insert(tuple,READ_OP_CONTEXT(rel_6_Live_op_ctxt));
}
}
();std::swap(rel_1_delta_Live, rel_2_new_Live);
rel_2_new_Live->purge();
iter++;
}
iter = 0;
rel_1_delta_Live->purge();
rel_2_new_Live->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"name","Live"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_6_Live);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_3(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
SignalHandler::instance()->setMsg(R"_(LiveReferredBy(to,from) :- 
   Reference(from,to),
   Live(from),
   Live(to).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-interpreter/datalog/ext-stg-gc.dl [45:1-48:12])_");
if(!(rel_6_Live->empty()) && !(rel_8_Reference->empty())) {
[&](){
auto part = rel_8_Reference->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_7_LiveReferredBy_op_ctxt,rel_7_LiveReferredBy->createContext());
CREATE_OP_CONTEXT(rel_8_Reference_op_ctxt,rel_8_Reference->createContext());
CREATE_OP_CONTEXT(rel_6_Live_op_ctxt,rel_6_Live->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
if( rel_6_Live->contains(Tuple<RamDomain,1>{{ramBitCast(env0[1])}},READ_OP_CONTEXT(rel_6_Live_op_ctxt)) && rel_6_Live->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_6_Live_op_ctxt))) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[1]),ramBitCast(env0[0])}};
rel_7_LiveReferredBy->insert(tuple,READ_OP_CONTEXT(rel_7_LiveReferredBy_op_ctxt));
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","to\tfrom"},{"name","LiveReferredBy"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"params\": [\"to\", \"from\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"i:number\", \"i:number\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_7_LiveReferredBy);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_4(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
SignalHandler::instance()->setMsg(R"_(All(from) :- 
   Reference(from,_).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-interpreter/datalog/ext-stg-gc.dl [27:1-29:23])_");
if(!(rel_8_Reference->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_8_Reference_op_ctxt,rel_8_Reference->createContext());
CREATE_OP_CONTEXT(rel_3_All_op_ctxt,rel_3_All->createContext());
for(const auto& env0 : *rel_8_Reference) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_3_All->insert(tuple,READ_OP_CONTEXT(rel_3_All_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(All(to) :- 
   Reference(_,to).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-interpreter/datalog/ext-stg-gc.dl [27:1-29:23])_");
if(!(rel_8_Reference->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_8_Reference_op_ctxt,rel_8_Reference->createContext());
CREATE_OP_CONTEXT(rel_3_All_op_ctxt,rel_3_All->createContext());
for(const auto& env0 : *rel_8_Reference) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[1])}};
rel_3_All->insert(tuple,READ_OP_CONTEXT(rel_3_All_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(All(val) :- 
   GCRoot(val).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-interpreter/datalog/ext-stg-gc.dl [31:1-32:15])_");
if(!(rel_5_GCRoot->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_5_GCRoot_op_ctxt,rel_5_GCRoot->createContext());
CREATE_OP_CONTEXT(rel_3_All_op_ctxt,rel_3_All->createContext());
for(const auto& env0 : *rel_5_GCRoot) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_3_All->insert(tuple,READ_OP_CONTEXT(rel_3_All_op_ctxt));
}
}
();}
if (performIO) rel_8_Reference->purge();
if (performIO) rel_5_GCRoot->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_5(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
SignalHandler::instance()->setMsg(R"_(Dead(val) :- 
   All(val),
   !Live(val).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-interpreter/datalog/ext-stg-gc.dl [37:1-39:14])_");
if(!(rel_3_All->empty())) {
[&](){
auto part = rel_3_All->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_4_Dead_op_ctxt,rel_4_Dead->createContext());
CREATE_OP_CONTEXT(rel_6_Live_op_ctxt,rel_6_Live->createContext());
CREATE_OP_CONTEXT(rel_3_All_op_ctxt,rel_3_All->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
if( !(rel_6_Live->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_6_Live_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_4_Dead->insert(tuple,READ_OP_CONTEXT(rel_4_Dead_op_ctxt));
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"name","Dead"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"i:number\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_Dead);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) rel_6_Live->purge();
if (performIO) rel_3_All->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
};
SouffleProgram *newInstance_ext_stg_gc(){return new Sf_ext_stg_gc;}
SymbolTable *getST_ext_stg_gc(SouffleProgram *p){return &reinterpret_cast<Sf_ext_stg_gc*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_ext_stg_gc: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_ext_stg_gc();
};
public:
factory_Sf_ext_stg_gc() : ProgramFactory("ext_stg_gc"){}
};
extern "C" {
factory_Sf_ext_stg_gc __factory_Sf_ext_stg_gc_instance;
}
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(ext-stg-gc.dl)",
R"()",
R"()",
false,
R"()",
4);
if (!opt.parse(argc,argv)) return 1;
souffle::Sf_ext_stg_gc obj;
#if defined(_OPENMP) 
obj.setNumThreads(opt.getNumJobs());

#endif
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
