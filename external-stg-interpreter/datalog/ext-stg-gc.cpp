
#include "souffle/CompiledSouffle.h"

namespace functors {
 extern "C" {
}
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
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
struct t_btree_ii__0_1__11__10 {
static constexpr Relation::arity_type Arity = 2;
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| ((ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))));
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
static inline std::string substr_wrapper(const std::string& str, std::size_t idx, std::size_t len) {
   std::string result; 
   try { result = str.substr(idx,len); } catch(...) { 
     std::cerr << "warning: wrong index position provided by substr(\"";
     std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
   } return result;
}
public:
// -- initialize symbol table --
SymbolTableImpl symTable;// -- initialize record table --
SpecializedRecordTable<0> recordTable{};
// -- Table: GCRoot
Own<t_btree_i__0__1> rel_1_GCRoot = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_1_GCRoot;
// -- Table: Reference
Own<t_btree_ii__0_1__11__10> rel_2_Reference = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<t_btree_ii__0_1__11__10> wrapper_rel_2_Reference;
// -- Table: LiveStep0
Own<t_btree_i__0__1> rel_3_LiveStep0 = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_3_LiveStep0;
// -- Table: @delta_LiveStep0
Own<t_btree_i__0__1> rel_4_delta_LiveStep0 = mk<t_btree_i__0__1>();
// -- Table: @new_LiveStep0
Own<t_btree_i__0__1> rel_5_new_LiveStep0 = mk<t_btree_i__0__1>();
// -- Table: MaybeDeadlockingThread
Own<t_btree_i__0__1> rel_6_MaybeDeadlockingThread = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_6_MaybeDeadlockingThread;
// -- Table: DeadlockingThread
Own<t_btree_i__0__1> rel_7_DeadlockingThread = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_7_DeadlockingThread;
// -- Table: Live
Own<t_btree_i__0__1> rel_8_Live = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_8_Live;
// -- Table: @delta_Live
Own<t_btree_i__0__1> rel_9_delta_Live = mk<t_btree_i__0__1>();
// -- Table: @new_Live
Own<t_btree_i__0__1> rel_10_new_Live = mk<t_btree_i__0__1>();
public:
Sf_ext_stg_gc()
: wrapper_rel_1_GCRoot(0, *rel_1_GCRoot, *this, "GCRoot", std::array<const char *,1>{{"s:symbol"}}, std::array<const char *,1>{{"val"}}, 0)
, wrapper_rel_2_Reference(1, *rel_2_Reference, *this, "Reference", std::array<const char *,2>{{"s:symbol","s:symbol"}}, std::array<const char *,2>{{"from","to"}}, 0)
, wrapper_rel_3_LiveStep0(2, *rel_3_LiveStep0, *this, "LiveStep0", std::array<const char *,1>{{"s:symbol"}}, std::array<const char *,1>{{"val"}}, 0)
, wrapper_rel_6_MaybeDeadlockingThread(3, *rel_6_MaybeDeadlockingThread, *this, "MaybeDeadlockingThread", std::array<const char *,1>{{"s:symbol"}}, std::array<const char *,1>{{"threadId"}}, 0)
, wrapper_rel_7_DeadlockingThread(4, *rel_7_DeadlockingThread, *this, "DeadlockingThread", std::array<const char *,1>{{"s:symbol"}}, std::array<const char *,1>{{"threadId"}}, 0)
, wrapper_rel_8_Live(5, *rel_8_Live, *this, "Live", std::array<const char *,1>{{"s:symbol"}}, std::array<const char *,1>{{"val"}}, 0)
{
addRelation("GCRoot", wrapper_rel_1_GCRoot, true, false);
addRelation("Reference", wrapper_rel_2_Reference, true, false);
addRelation("LiveStep0", wrapper_rel_3_LiveStep0, false, false);
addRelation("MaybeDeadlockingThread", wrapper_rel_6_MaybeDeadlockingThread, true, false);
addRelation("DeadlockingThread", wrapper_rel_7_DeadlockingThread, false, true);
addRelation("Live", wrapper_rel_8_Live, false, true);
}
~Sf_ext_stg_gc() {
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
    if (0 < getNumThreads()) { omp_set_num_threads(static_cast<int>(getNumThreads())); }
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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","threadId"},{"auxArity","0"},{"name","DeadlockingThread"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"threadId\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_7_DeadlockingThread);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"auxArity","0"},{"name","Live"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_8_Live);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"auxArity","0"},{"fact-dir","."},{"name","GCRoot"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_GCRoot);
} catch (std::exception& e) {std::cerr << "Error loading GCRoot data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","threadId"},{"auxArity","0"},{"fact-dir","."},{"name","MaybeDeadlockingThread"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"threadId\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_6_MaybeDeadlockingThread);
} catch (std::exception& e) {std::cerr << "Error loading MaybeDeadlockingThread data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","from\tto"},{"auxArity","0"},{"fact-dir","."},{"name","Reference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"from\", \"to\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_Reference);
} catch (std::exception& e) {std::cerr << "Error loading Reference data: " << e.what() << '\n';}
}
public:
void dumpInputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "GCRoot";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_GCRoot);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "MaybeDeadlockingThread";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_6_MaybeDeadlockingThread);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "Reference";
rwOperation["types"] = "{\"relation\": {\"arity\": 2, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_2_Reference);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "DeadlockingThread";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_7_DeadlockingThread);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "Live";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_8_Live);
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
fatal("unknown subroutine");
}
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_0(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"auxArity","0"},{"fact-dir","."},{"name","GCRoot"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_1_GCRoot);
} catch (std::exception& e) {std::cerr << "Error loading GCRoot data: " << e.what() << '\n';}
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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","from\tto"},{"auxArity","0"},{"fact-dir","."},{"name","Reference"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 2, \"params\": [\"from\", \"to\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 2, \"types\": [\"s:symbol\", \"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_2_Reference);
} catch (std::exception& e) {std::cerr << "Error loading Reference data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_2(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(LiveStep0(ref) :- 
   GCRoot(ref).
in file ext-stg-gc.dl [26:1-26:31])_");
if(!(rel_1_GCRoot->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_3_LiveStep0_op_ctxt,rel_3_LiveStep0->createContext());
CREATE_OP_CONTEXT(rel_1_GCRoot_op_ctxt,rel_1_GCRoot->createContext());
for(const auto& env0 : *rel_1_GCRoot) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_3_LiveStep0->insert(tuple,READ_OP_CONTEXT(rel_3_LiveStep0_op_ctxt));
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_3_LiveStep0_op_ctxt,rel_3_LiveStep0->createContext());
CREATE_OP_CONTEXT(rel_4_delta_LiveStep0_op_ctxt,rel_4_delta_LiveStep0->createContext());
for(const auto& env0 : *rel_3_LiveStep0) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_4_delta_LiveStep0->insert(tuple,READ_OP_CONTEXT(rel_4_delta_LiveStep0_op_ctxt));
}
}
();iter = 0;
for(;;) {
signalHandler->setMsg(R"_(LiveStep0(to) :- 
   LiveStep0(from),
   Reference(from,to).
in file ext-stg-gc.dl [28:1-30:23])_");
if(!(rel_4_delta_LiveStep0->empty()) && !(rel_2_Reference->empty())) {
[&](){
auto part = rel_4_delta_LiveStep0->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_3_LiveStep0_op_ctxt,rel_3_LiveStep0->createContext());
CREATE_OP_CONTEXT(rel_4_delta_LiveStep0_op_ctxt,rel_4_delta_LiveStep0->createContext());
CREATE_OP_CONTEXT(rel_5_new_LiveStep0_op_ctxt,rel_5_new_LiveStep0->createContext());
CREATE_OP_CONTEXT(rel_2_Reference_op_ctxt,rel_2_Reference->createContext());

                   #if defined _OPENMP && _OPENMP < 200805
                           auto count = std::distance(part.begin(), part.end());
                           auto base = part.begin();
                           pfor(int index  = 0; index < count; index++) {
                               auto it = base + index;
                   #else
                           pfor(auto it = part.begin(); it < part.end(); it++) {
                   #endif
                   try{
for(const auto& env0 : *it) {
auto range = rel_2_Reference->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_2_Reference_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_3_LiveStep0->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_3_LiveStep0_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_5_new_LiveStep0->insert(tuple,READ_OP_CONTEXT(rel_5_new_LiveStep0_op_ctxt));
}
}
}
} catch(std::exception &e) { signalHandler->error(e.what());}
}
PARALLEL_END
}
();}
if(rel_5_new_LiveStep0->empty()) break;
[&](){
CREATE_OP_CONTEXT(rel_3_LiveStep0_op_ctxt,rel_3_LiveStep0->createContext());
CREATE_OP_CONTEXT(rel_5_new_LiveStep0_op_ctxt,rel_5_new_LiveStep0->createContext());
for(const auto& env0 : *rel_5_new_LiveStep0) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_3_LiveStep0->insert(tuple,READ_OP_CONTEXT(rel_3_LiveStep0_op_ctxt));
}
}
();std::swap(rel_4_delta_LiveStep0, rel_5_new_LiveStep0);
rel_5_new_LiveStep0->purge();
iter++;
}
iter = 0;
rel_4_delta_LiveStep0->purge();
rel_5_new_LiveStep0->purge();
if (pruneImdtRels) rel_1_GCRoot->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_3(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","threadId"},{"auxArity","0"},{"fact-dir","."},{"name","MaybeDeadlockingThread"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"threadId\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_6_MaybeDeadlockingThread);
} catch (std::exception& e) {std::cerr << "Error loading MaybeDeadlockingThread data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_4(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(DeadlockingThread(tid) :- 
   MaybeDeadlockingThread(tid),
   !LiveStep0(tid).
in file ext-stg-gc.dl [41:1-43:19])_");
if(!(rel_6_MaybeDeadlockingThread->empty())) {
[&](){
auto part = rel_6_MaybeDeadlockingThread->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_3_LiveStep0_op_ctxt,rel_3_LiveStep0->createContext());
CREATE_OP_CONTEXT(rel_6_MaybeDeadlockingThread_op_ctxt,rel_6_MaybeDeadlockingThread->createContext());
CREATE_OP_CONTEXT(rel_7_DeadlockingThread_op_ctxt,rel_7_DeadlockingThread->createContext());

                   #if defined _OPENMP && _OPENMP < 200805
                           auto count = std::distance(part.begin(), part.end());
                           auto base = part.begin();
                           pfor(int index  = 0; index < count; index++) {
                               auto it = base + index;
                   #else
                           pfor(auto it = part.begin(); it < part.end(); it++) {
                   #endif
                   try{
for(const auto& env0 : *it) {
if( !(rel_3_LiveStep0->contains(Tuple<RamDomain,1>{{ramBitCast(env0[0])}},READ_OP_CONTEXT(rel_3_LiveStep0_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_7_DeadlockingThread->insert(tuple,READ_OP_CONTEXT(rel_7_DeadlockingThread_op_ctxt));
}
}
} catch(std::exception &e) { signalHandler->error(e.what());}
}
PARALLEL_END
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","threadId"},{"auxArity","0"},{"name","DeadlockingThread"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"threadId\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_7_DeadlockingThread);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (pruneImdtRels) rel_6_MaybeDeadlockingThread->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_5(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(Live(tid) :- 
   DeadlockingThread(tid).
in file ext-stg-gc.dl [45:1-45:37])_");
if(!(rel_7_DeadlockingThread->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_7_DeadlockingThread_op_ctxt,rel_7_DeadlockingThread->createContext());
CREATE_OP_CONTEXT(rel_8_Live_op_ctxt,rel_8_Live->createContext());
for(const auto& env0 : *rel_7_DeadlockingThread) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_8_Live->insert(tuple,READ_OP_CONTEXT(rel_8_Live_op_ctxt));
}
}
();}
signalHandler->setMsg(R"_(Live(ref) :- 
   LiveStep0(ref).
in file ext-stg-gc.dl [46:1-46:29])_");
if(!(rel_3_LiveStep0->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_3_LiveStep0_op_ctxt,rel_3_LiveStep0->createContext());
CREATE_OP_CONTEXT(rel_8_Live_op_ctxt,rel_8_Live->createContext());
for(const auto& env0 : *rel_3_LiveStep0) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_8_Live->insert(tuple,READ_OP_CONTEXT(rel_8_Live_op_ctxt));
}
}
();}
[&](){
CREATE_OP_CONTEXT(rel_8_Live_op_ctxt,rel_8_Live->createContext());
CREATE_OP_CONTEXT(rel_9_delta_Live_op_ctxt,rel_9_delta_Live->createContext());
for(const auto& env0 : *rel_8_Live) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_9_delta_Live->insert(tuple,READ_OP_CONTEXT(rel_9_delta_Live_op_ctxt));
}
}
();iter = 0;
for(;;) {
signalHandler->setMsg(R"_(Live(to) :- 
   Live(from),
   Reference(from,to).
in file ext-stg-gc.dl [47:1-49:23])_");
if(!(rel_9_delta_Live->empty()) && !(rel_2_Reference->empty())) {
[&](){
auto part = rel_9_delta_Live->partition();
PARALLEL_START
CREATE_OP_CONTEXT(rel_2_Reference_op_ctxt,rel_2_Reference->createContext());
CREATE_OP_CONTEXT(rel_8_Live_op_ctxt,rel_8_Live->createContext());
CREATE_OP_CONTEXT(rel_9_delta_Live_op_ctxt,rel_9_delta_Live->createContext());
CREATE_OP_CONTEXT(rel_10_new_Live_op_ctxt,rel_10_new_Live->createContext());

                   #if defined _OPENMP && _OPENMP < 200805
                           auto count = std::distance(part.begin(), part.end());
                           auto base = part.begin();
                           pfor(int index  = 0; index < count; index++) {
                               auto it = base + index;
                   #else
                           pfor(auto it = part.begin(); it < part.end(); it++) {
                   #endif
                   try{
for(const auto& env0 : *it) {
auto range = rel_2_Reference->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_2_Reference_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_8_Live->contains(Tuple<RamDomain,1>{{ramBitCast(env1[1])}},READ_OP_CONTEXT(rel_8_Live_op_ctxt)))) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_10_new_Live->insert(tuple,READ_OP_CONTEXT(rel_10_new_Live_op_ctxt));
}
}
}
} catch(std::exception &e) { signalHandler->error(e.what());}
}
PARALLEL_END
}
();}
if(rel_10_new_Live->empty()) break;
[&](){
CREATE_OP_CONTEXT(rel_8_Live_op_ctxt,rel_8_Live->createContext());
CREATE_OP_CONTEXT(rel_10_new_Live_op_ctxt,rel_10_new_Live->createContext());
for(const auto& env0 : *rel_10_new_Live) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_8_Live->insert(tuple,READ_OP_CONTEXT(rel_8_Live_op_ctxt));
}
}
();std::swap(rel_9_delta_Live, rel_10_new_Live);
rel_10_new_Live->purge();
iter++;
}
iter = 0;
rel_9_delta_Live->purge();
rel_10_new_Live->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","val"},{"auxArity","0"},{"name","Live"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"val\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_8_Live);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (pruneImdtRels) rel_8_Live->purge();
if (pruneImdtRels) rel_3_LiveStep0->purge();
if (pruneImdtRels) rel_2_Reference->purge();
if (pruneImdtRels) rel_7_DeadlockingThread->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
};
SouffleProgram *newInstance_ext_stg_gc(){return new Sf_ext_stg_gc;}
SymbolTable *getST_ext_stg_gc(SouffleProgram *p){return &reinterpret_cast<Sf_ext_stg_gc*>(p)->getSymbolTable();}

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
8);
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
