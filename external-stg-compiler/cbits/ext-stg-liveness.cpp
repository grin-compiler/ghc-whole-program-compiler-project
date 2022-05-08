
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_ii__0_1__11__10 {
static constexpr Relation::arity_type Arity = 2;
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
struct t_btree_i__0__1 {
static constexpr Relation::arity_type Arity = 1;
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
struct t_btree_ii__1_0__11__01 {
static constexpr Relation::arity_type Arity = 2;
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :((ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))|| (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1])) && ((ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]))&&(ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]));
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
range<t_ind_0::iterator> lowerUpperRange_01(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_01(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_01(lower,upper,h);
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
static inline std::string substr_wrapper(const std::string& str, std::size_t idx, std::size_t len) {
   std::string result; 
   try { result = str.substr(idx,len); } catch(...) { 
     std::cerr << "warning: wrong index position provided by substr(\"";
     std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
   } return result;
}
public:
// -- initialize symbol table --
SymbolTable symTable;// -- initialize record table --
SpecializedRecordTable<0> recordTable{};
// -- Table: DataConReference
Own<t_btree_ii__0_1__11__10> rel_1_DataConReference = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<t_btree_ii__0_1__11__10> wrapper_rel_1_DataConReference;
// -- Table: FunReference
Own<t_btree_ii__0_1__11__10> rel_2_FunReference = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<t_btree_ii__0_1__11__10> wrapper_rel_2_FunReference;
// -- Table: LiveSource
Own<t_btree_i__0__1> rel_3_LiveSource = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_3_LiveSource;
// -- Table: LiveFunName
Own<t_btree_i__0__1> rel_4_LiveFunName = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_4_LiveFunName;
// -- Table: @delta_LiveFunName
Own<t_btree_i__0__1> rel_5_delta_LiveFunName = mk<t_btree_i__0__1>();
// -- Table: @new_LiveFunName
Own<t_btree_i__0__1> rel_6_new_LiveFunName = mk<t_btree_i__0__1>();
// -- Table: TyCon
Own<t_btree_ii__1_0__11__01> rel_7_TyCon = mk<t_btree_ii__1_0__11__01>();
souffle::RelationWrapper<t_btree_ii__1_0__11__01> wrapper_rel_7_TyCon;
// -- Table: LiveDataConName
Own<t_btree_i__0__1> rel_8_LiveDataConName = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_8_LiveDataConName;
// -- Table: TyConReference
Own<t_btree_ii__0_1__11__10> rel_9_TyConReference = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<t_btree_ii__0_1__11__10> wrapper_rel_9_TyConReference;
// -- Table: LiveTyConName
Own<t_btree_i__0__1> rel_10_LiveTyConName = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_10_LiveTyConName;
public:
Sf_ext_stg_liveness()
: wrapper_rel_1_DataConReference(0, *rel_1_DataConReference, *this, "DataConReference", std::array<const char *,2>{{"s:Name","s:Name"}}, std::array<const char *,2>{{"fun","datacon"}}, 0)
, wrapper_rel_2_FunReference(1, *rel_2_FunReference, *this, "FunReference", std::array<const char *,2>{{"s:Name","s:Name"}}, std::array<const char *,2>{{"fun","funref"}}, 0)
, wrapper_rel_3_LiveSource(2, *rel_3_LiveSource, *this, "LiveSource", std::array<const char *,1>{{"s:Name"}}, std::array<const char *,1>{{"fun"}}, 0)
, wrapper_rel_4_LiveFunName(3, *rel_4_LiveFunName, *this, "LiveFunName", std::array<const char *,1>{{"s:Name"}}, std::array<const char *,1>{{"fun"}}, 0)
, wrapper_rel_7_TyCon(4, *rel_7_TyCon, *this, "TyCon", std::array<const char *,2>{{"s:Name","s:Name"}}, std::array<const char *,2>{{"tycon","datacon"}}, 0)
, wrapper_rel_8_LiveDataConName(5, *rel_8_LiveDataConName, *this, "LiveDataConName", std::array<const char *,1>{{"s:Name"}}, std::array<const char *,1>{{"datacon"}}, 0)
, wrapper_rel_9_TyConReference(6, *rel_9_TyConReference, *this, "TyConReference", std::array<const char *,2>{{"s:Name","s:Name"}}, std::array<const char *,2>{{"fun","tycon"}}, 0)
, wrapper_rel_10_LiveTyConName(7, *rel_10_LiveTyConName, *this, "LiveTyConName", std::array<const char *,1>{{"s:Name"}}, std::array<const char *,1>{{"tycon"}}, 0)
{
addRelation("DataConReference", wrapper_rel_1_DataConReference, true, false);
addRelation("FunReference", wrapper_rel_2_FunReference, true, false);
addRelation("LiveSource", wrapper_rel_3_LiveSource, true, false);
addRelation("LiveFunName", wrapper_rel_4_LiveFunName, false, true);
addRelation("TyCon", wrapper_rel_7_TyCon, true, false);
addRelation("LiveDataConName", wrapper_rel_8_LiveDataConName, false, true);
addRelation("TyConReference", wrapper_rel_9_TyConReference, true, false);
addRelation("LiveTyConName", wrapper_rel_10_LiveTyConName, false, true);
}
~Sf_ext_stg_liveness() {
}

private:
std::string             inputDirectory;
std::string             outputDirectory;
SignalHandler*          signalHandler {SignalHandler::instance()};
std::atomic<RamDomain>  ctr {};
std::atomic<std::size_t>     iter {};

void runFunction(std::string  inputDirectoryArg,
                 std::string  outputDirectoryArg,
                 bool         performIOArg,
                 bool         pruneImdtRelsArg) {
    this->inputDirectory  = std::move(inputDirectoryArg);
    this->outputDirectory = std::move(outputDirectoryArg);
    this->performIO       = performIOArg;
    this->pruneImdtRels   = pruneImdtRelsArg; 

    // set default threads (in embedded mode)
    // if this is not set, and omp is used, the default omp setting of number of cores is used.
#if defined(_OPENMP)
    if (0 < getNumThreads()) { omp_set_num_threads(getNumThreads()); }
#endif

    signalHandler->set();
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
signalHandler->reset();
}
public:
void run() override { runFunction("", "", false, false); }
public:
void runAll(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "", bool performIOArg=true, bool pruneImdtRelsArg=true) override { runFunction(inputDirectoryArg, outputDirectoryArg, performIOArg, pruneImdtRelsArg);
}
public:
void printAll(std::string outputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun"},{"auxArity","0"},{"name","LiveFunName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"fun\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_LiveFunName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","datacon"},{"auxArity","0"},{"name","LiveDataConName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"datacon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_8_LiveDataConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","tycon"},{"auxArity","0"},{"name","LiveTyConName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"tycon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_10_LiveTyConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\tdatacon"},{"auxArity","0"},{"fact-dir","."},{"name","DataConReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"fun\", \"datacon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_DataConReference);
} catch (std::exception& e) {std::cerr << "Error loading DataConReference data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\tfunref"},{"auxArity","0"},{"fact-dir","."},{"name","FunReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"fun\", \"funref\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_FunReference);
} catch (std::exception& e) {std::cerr << "Error loading FunReference data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","tycon\tdatacon"},{"auxArity","0"},{"fact-dir","."},{"name","TyCon"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"tycon\", \"datacon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_7_TyCon);
} catch (std::exception& e) {std::cerr << "Error loading TyCon data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun"},{"auxArity","0"},{"fact-dir","."},{"name","LiveSource"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"fun\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:Name\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_LiveSource);
} catch (std::exception& e) {std::cerr << "Error loading LiveSource data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\ttycon"},{"auxArity","0"},{"fact-dir","."},{"name","TyConReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"fun\", \"tycon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_9_TyConReference);
} catch (std::exception& e) {std::cerr << "Error loading TyConReference data: " << e.what() << '\n';}
}
public:
void dumpInputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "DataConReference";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_DataConReference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "FunReference";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_FunReference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "TyCon";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_7_TyCon);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "LiveSource";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_3_LiveSource);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "TyConReference";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:Name\", \"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_9_TyConReference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "LiveFunName";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_LiveFunName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "LiveDataConName";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_8_LiveDataConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "LiveTyConName";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:Name\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_10_LiveTyConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
RecordTable& getRecordTable() override {
return recordTable;
}
void setNumThreads(std::size_t numThreadsValue) override {
SouffleProgram::setNumThreads(numThreadsValue);
symTable.setNumLanes(getNumThreads());
recordTable.setNumLanes(getNumThreads());
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
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_0(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\tdatacon"},{"auxArity","0"},{"fact-dir","."},{"name","DataConReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"fun\", \"datacon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_DataConReference);
} catch (std::exception& e) {std::cerr << "Error loading DataConReference data: " << e.what() << '\n';}
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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\tfunref"},{"auxArity","0"},{"fact-dir","."},{"name","FunReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"fun\", \"funref\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_FunReference);
} catch (std::exception& e) {std::cerr << "Error loading FunReference data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_2(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun"},{"auxArity","0"},{"fact-dir","."},{"name","LiveSource"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"fun\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_LiveSource);
} catch (std::exception& e) {std::cerr << "Error loading LiveSource data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_3(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(LiveFunName(fun) :- 
   LiveSource(fun).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [32:1-33:19])_");
if(!(rel_3_LiveSource->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,rel_4_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_3_LiveSource_op_ctxt,rel_3_LiveSource->createContext());
for(const auto& env0 : *rel_3_LiveSource) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_4_LiveFunName->insert(tuple,READ_OP_CONTEXT(rel_4_LiveFunName_op_ctxt));
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_5_delta_LiveFunName_op_ctxt,rel_5_delta_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,rel_4_LiveFunName->createContext());
for(const auto& env0 : *rel_4_LiveFunName) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_5_delta_LiveFunName->insert(tuple,READ_OP_CONTEXT(rel_5_delta_LiveFunName_op_ctxt));
}
}
();iter = 0;
for(;;) {
signalHandler->setMsg(R"_(LiveFunName(ref) :- 
   LiveFunName(fun),
   FunReference(fun,ref).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [35:1-37:26])_");
if(!(rel_5_delta_LiveFunName->empty()) && !(rel_2_FunReference->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_5_delta_LiveFunName_op_ctxt,rel_5_delta_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,rel_4_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_2_FunReference_op_ctxt,rel_2_FunReference->createContext());
CREATE_OP_CONTEXT(rel_6_new_LiveFunName_op_ctxt,rel_6_new_LiveFunName->createContext());
for(const auto& env0 : *rel_5_delta_LiveFunName) {
auto range = rel_2_FunReference->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_2_FunReference_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_4_LiveFunName->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_4_LiveFunName_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_6_new_LiveFunName->insert(tuple,READ_OP_CONTEXT(rel_6_new_LiveFunName_op_ctxt));
}
}
}
}
();}
if(rel_6_new_LiveFunName->empty()) break;
[&](){
CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,rel_4_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_6_new_LiveFunName_op_ctxt,rel_6_new_LiveFunName->createContext());
for(const auto& env0 : *rel_6_new_LiveFunName) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_4_LiveFunName->insert(tuple,READ_OP_CONTEXT(rel_4_LiveFunName_op_ctxt));
}
}
();std::swap(rel_5_delta_LiveFunName, rel_6_new_LiveFunName);
rel_6_new_LiveFunName->purge();
iter++;
}
iter = 0;
rel_5_delta_LiveFunName->purge();
rel_6_new_LiveFunName->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun"},{"auxArity","0"},{"name","LiveFunName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"fun\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_LiveFunName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (pruneImdtRels) rel_2_FunReference->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_4(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","tycon\tdatacon"},{"auxArity","0"},{"fact-dir","."},{"name","TyCon"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"tycon\", \"datacon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_7_TyCon);
} catch (std::exception& e) {std::cerr << "Error loading TyCon data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_5(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(LiveDataConName(fun) :- 
   LiveSource(fun).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [40:1-41:19])_");
if(!(rel_3_LiveSource->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt,rel_8_LiveDataConName->createContext());
CREATE_OP_CONTEXT(rel_3_LiveSource_op_ctxt,rel_3_LiveSource->createContext());
for(const auto& env0 : *rel_3_LiveSource) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_8_LiveDataConName->insert(tuple,READ_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt));
}
}
();}
signalHandler->setMsg(R"_(LiveDataConName(datacon) :- 
   LiveFunName(fun),
   DataConReference(fun,datacon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [43:1-45:34])_");
if(!(rel_4_LiveFunName->empty()) && !(rel_1_DataConReference->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,rel_4_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt,rel_8_LiveDataConName->createContext());
CREATE_OP_CONTEXT(rel_1_DataConReference_op_ctxt,rel_1_DataConReference->createContext());
for(const auto& env0 : *rel_4_LiveFunName) {
auto range = rel_1_DataConReference->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_1_DataConReference_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_8_LiveDataConName->insert(tuple,READ_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt));
}
}
}
();}
signalHandler->setMsg(R"_(LiveDataConName(datacon) :- 
   TyCon(_,datacon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [59:1-60:21])_");
if(!(rel_7_TyCon->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt,rel_8_LiveDataConName->createContext());
CREATE_OP_CONTEXT(rel_7_TyCon_op_ctxt,rel_7_TyCon->createContext());
for(const auto& env0 : *rel_7_TyCon) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[1])}};
rel_8_LiveDataConName->insert(tuple,READ_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt));
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","datacon"},{"auxArity","0"},{"name","LiveDataConName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"datacon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_8_LiveDataConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (pruneImdtRels) rel_1_DataConReference->purge();
if (pruneImdtRels) rel_3_LiveSource->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_6(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","fun\ttycon"},{"auxArity","0"},{"fact-dir","."},{"name","TyConReference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"fun\", \"tycon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:Name\", \"s:Name\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_9_TyConReference);
} catch (std::exception& e) {std::cerr << "Error loading TyConReference data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_7(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(LiveTyConName(tycon) :- 
   LiveDataConName(datacon),
   TyCon(tycon,datacon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [48:1-50:25])_");
if(!(rel_8_LiveDataConName->empty()) && !(rel_7_TyCon->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_10_LiveTyConName_op_ctxt,rel_10_LiveTyConName->createContext());
CREATE_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt,rel_8_LiveDataConName->createContext());
CREATE_OP_CONTEXT(rel_7_TyCon_op_ctxt,rel_7_TyCon->createContext());
for(const auto& env0 : *rel_8_LiveDataConName) {
auto range = rel_7_TyCon->lowerUpperRange_01(Tuple<RamDomain,2>{{ramBitCast<RamDomain>(MIN_RAM_SIGNED), ramBitCast(env0[0])}},Tuple<RamDomain,2>{{ramBitCast<RamDomain>(MAX_RAM_SIGNED), ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_7_TyCon_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[0])}};
rel_10_LiveTyConName->insert(tuple,READ_OP_CONTEXT(rel_10_LiveTyConName_op_ctxt));
}
}
}
();}
signalHandler->setMsg(R"_(LiveTyConName(tycon) :- 
   LiveFunName(fun),
   TyConReference(fun,tycon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [52:1-54:30])_");
if(!(rel_4_LiveFunName->empty()) && !(rel_9_TyConReference->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_10_LiveTyConName_op_ctxt,rel_10_LiveTyConName->createContext());
CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,rel_4_LiveFunName->createContext());
CREATE_OP_CONTEXT(rel_9_TyConReference_op_ctxt,rel_9_TyConReference->createContext());
for(const auto& env0 : *rel_4_LiveFunName) {
auto range = rel_9_TyConReference->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_9_TyConReference_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_10_LiveTyConName->insert(tuple,READ_OP_CONTEXT(rel_10_LiveTyConName_op_ctxt));
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","tycon"},{"auxArity","0"},{"name","LiveTyConName"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"tycon\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:Name\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_10_LiveTyConName);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (pruneImdtRels) rel_7_TyCon->purge();
if (pruneImdtRels) rel_9_TyConReference->purge();
if (pruneImdtRels) rel_4_LiveFunName->purge();
if (pruneImdtRels) rel_8_LiveDataConName->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
};
SouffleProgram *newInstance_ext_stg_liveness(){return new Sf_ext_stg_liveness;}
SymbolTable *getST_ext_stg_liveness(SouffleProgram *p){return &reinterpret_cast<Sf_ext_stg_liveness*>(p)->getSymbolTable();}

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
1);
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
