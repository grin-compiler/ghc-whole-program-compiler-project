#include "souffle/CompiledSouffle.h"

extern "C" {}

namespace souffle {
using namespace ram;
struct t_btree_2__0_1__1__3 {
  using t_tuple = Tuple<RamDomain, 2>;
  using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0, 1>>;
  t_ind_0 ind_0;
  using iterator = t_ind_0::iterator;
  struct context {
    t_ind_0::operation_hints hints_0;
  };
  context createContext() { return context(); }
  bool insert(const t_tuple &t) {
    context h;
    return insert(t, h);
  }
  bool insert(const t_tuple &t, context &h) {
    if (ind_0.insert(t, h.hints_0)) {
      return true;
    } else
      return false;
  }
  bool insert(const RamDomain *ramDomain) {
    RamDomain data[2];
    std::copy(ramDomain, ramDomain + 2, data);
    const t_tuple &tuple = reinterpret_cast<const t_tuple &>(data);
    context h;
    return insert(tuple, h);
  }
  bool insert(RamDomain a0, RamDomain a1) {
    RamDomain data[2] = {a0, a1};
    return insert(data);
  }
  template <typename T> void insertAll(T &other) {
    for (auto const &cur : other) {
      insert(cur);
    }
  }
  void insertAll(t_btree_2__0_1__1__3 &other) { ind_0.insertAll(other.ind_0); }
  bool contains(const t_tuple &t, context &h) const {
    return ind_0.contains(t, h.hints_0);
  }
  bool contains(const t_tuple &t) const {
    context h;
    return contains(t, h);
  }
  std::size_t size() const { return ind_0.size(); }
  iterator find(const t_tuple &t, context &h) const {
    return ind_0.find(t, h.hints_0);
  }
  iterator find(const t_tuple &t) const {
    context h;
    return find(t, h);
  }
  range<iterator> equalRange_0(const t_tuple &t, context &h) const {
    return range<iterator>(ind_0.begin(), ind_0.end());
  }
  range<iterator> equalRange_0(const t_tuple &t) const {
    return range<iterator>(ind_0.begin(), ind_0.end());
  }
  range<t_ind_0::iterator> equalRange_1(const t_tuple &t, context &h) const {
    t_tuple low(t);
    t_tuple high(t);
    low[1] = MIN_RAM_DOMAIN;
    high[1] = MAX_RAM_DOMAIN;
    return make_range(ind_0.lower_bound(low, h.hints_0),
                      ind_0.upper_bound(high, h.hints_0));
  }
  range<t_ind_0::iterator> equalRange_1(const t_tuple &t) const {
    context h;
    return equalRange_1(t, h);
  }
  range<t_ind_0::iterator> equalRange_3(const t_tuple &t, context &h) const {
    auto pos = ind_0.find(t, h.hints_0);
    auto fin = ind_0.end();
    if (pos != fin) {
      fin = pos;
      ++fin;
    }
    return make_range(pos, fin);
  }
  range<t_ind_0::iterator> equalRange_3(const t_tuple &t) const {
    context h;
    return equalRange_3(t, h);
  }
  bool empty() const { return ind_0.empty(); }
  std::vector<range<iterator>> partition() const {
    return ind_0.getChunks(400);
  }
  void purge() { ind_0.clear(); }
  iterator begin() const { return ind_0.begin(); }
  iterator end() const { return ind_0.end(); }
  void printHintStatistics(std::ostream &o, const std::string prefix) const {
    const auto &stats_0 = ind_0.getHintStatistics();
    o << prefix << "arity 2 direct b-tree index [0,1]: (hits/misses/total)\n";
    o << prefix << "Insert: " << stats_0.inserts.getHits() << "/"
      << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses()
      << "\n";
    o << prefix << "Contains: " << stats_0.contains.getHits() << "/"
      << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses()
      << "\n";
    o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/"
      << stats_0.lower_bound.getMisses() << "/"
      << stats_0.lower_bound.getAccesses() << "\n";
    o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/"
      << stats_0.upper_bound.getMisses() << "/"
      << stats_0.upper_bound.getAccesses() << "\n";
  }
};
struct t_btree_1__0__1 {
  using t_tuple = Tuple<RamDomain, 1>;
  using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0>>;
  t_ind_0 ind_0;
  using iterator = t_ind_0::iterator;
  struct context {
    t_ind_0::operation_hints hints_0;
  };
  context createContext() { return context(); }
  bool insert(const t_tuple &t) {
    context h;
    return insert(t, h);
  }
  bool insert(const t_tuple &t, context &h) {
    if (ind_0.insert(t, h.hints_0)) {
      return true;
    } else
      return false;
  }
  bool insert(const RamDomain *ramDomain) {
    RamDomain data[1];
    std::copy(ramDomain, ramDomain + 1, data);
    const t_tuple &tuple = reinterpret_cast<const t_tuple &>(data);
    context h;
    return insert(tuple, h);
  }
  bool insert(RamDomain a0) {
    RamDomain data[1] = {a0};
    return insert(data);
  }
  template <typename T> void insertAll(T &other) {
    for (auto const &cur : other) {
      insert(cur);
    }
  }
  void insertAll(t_btree_1__0__1 &other) { ind_0.insertAll(other.ind_0); }
  bool contains(const t_tuple &t, context &h) const {
    return ind_0.contains(t, h.hints_0);
  }
  bool contains(const t_tuple &t) const {
    context h;
    return contains(t, h);
  }
  std::size_t size() const { return ind_0.size(); }
  iterator find(const t_tuple &t, context &h) const {
    return ind_0.find(t, h.hints_0);
  }
  iterator find(const t_tuple &t) const {
    context h;
    return find(t, h);
  }
  range<iterator> equalRange_0(const t_tuple &t, context &h) const {
    return range<iterator>(ind_0.begin(), ind_0.end());
  }
  range<iterator> equalRange_0(const t_tuple &t) const {
    return range<iterator>(ind_0.begin(), ind_0.end());
  }
  range<t_ind_0::iterator> equalRange_1(const t_tuple &t, context &h) const {
    auto pos = ind_0.find(t, h.hints_0);
    auto fin = ind_0.end();
    if (pos != fin) {
      fin = pos;
      ++fin;
    }
    return make_range(pos, fin);
  }
  range<t_ind_0::iterator> equalRange_1(const t_tuple &t) const {
    context h;
    return equalRange_1(t, h);
  }
  bool empty() const { return ind_0.empty(); }
  std::vector<range<iterator>> partition() const {
    return ind_0.getChunks(400);
  }
  void purge() { ind_0.clear(); }
  iterator begin() const { return ind_0.begin(); }
  iterator end() const { return ind_0.end(); }
  void printHintStatistics(std::ostream &o, const std::string prefix) const {
    const auto &stats_0 = ind_0.getHintStatistics();
    o << prefix << "arity 1 direct b-tree index [0]: (hits/misses/total)\n";
    o << prefix << "Insert: " << stats_0.inserts.getHits() << "/"
      << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses()
      << "\n";
    o << prefix << "Contains: " << stats_0.contains.getHits() << "/"
      << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses()
      << "\n";
    o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/"
      << stats_0.lower_bound.getMisses() << "/"
      << stats_0.lower_bound.getAccesses() << "\n";
    o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/"
      << stats_0.upper_bound.getMisses() << "/"
      << stats_0.upper_bound.getAccesses() << "\n";
  }
};
struct t_btree_2__1_0__2__3 {
  using t_tuple = Tuple<RamDomain, 2>;
  using t_ind_0 = btree_set<t_tuple, index_utils::comparator<1, 0>>;
  t_ind_0 ind_0;
  using iterator = t_ind_0::iterator;
  struct context {
    t_ind_0::operation_hints hints_0;
  };
  context createContext() { return context(); }
  bool insert(const t_tuple &t) {
    context h;
    return insert(t, h);
  }
  bool insert(const t_tuple &t, context &h) {
    if (ind_0.insert(t, h.hints_0)) {
      return true;
    } else
      return false;
  }
  bool insert(const RamDomain *ramDomain) {
    RamDomain data[2];
    std::copy(ramDomain, ramDomain + 2, data);
    const t_tuple &tuple = reinterpret_cast<const t_tuple &>(data);
    context h;
    return insert(tuple, h);
  }
  bool insert(RamDomain a0, RamDomain a1) {
    RamDomain data[2] = {a0, a1};
    return insert(data);
  }
  template <typename T> void insertAll(T &other) {
    for (auto const &cur : other) {
      insert(cur);
    }
  }
  void insertAll(t_btree_2__1_0__2__3 &other) { ind_0.insertAll(other.ind_0); }
  bool contains(const t_tuple &t, context &h) const {
    return ind_0.contains(t, h.hints_0);
  }
  bool contains(const t_tuple &t) const {
    context h;
    return contains(t, h);
  }
  std::size_t size() const { return ind_0.size(); }
  iterator find(const t_tuple &t, context &h) const {
    return ind_0.find(t, h.hints_0);
  }
  iterator find(const t_tuple &t) const {
    context h;
    return find(t, h);
  }
  range<iterator> equalRange_0(const t_tuple &t, context &h) const {
    return range<iterator>(ind_0.begin(), ind_0.end());
  }
  range<iterator> equalRange_0(const t_tuple &t) const {
    return range<iterator>(ind_0.begin(), ind_0.end());
  }
  range<t_ind_0::iterator> equalRange_2(const t_tuple &t, context &h) const {
    t_tuple low(t);
    t_tuple high(t);
    low[0] = MIN_RAM_DOMAIN;
    high[0] = MAX_RAM_DOMAIN;
    return make_range(ind_0.lower_bound(low, h.hints_0),
                      ind_0.upper_bound(high, h.hints_0));
  }
  range<t_ind_0::iterator> equalRange_2(const t_tuple &t) const {
    context h;
    return equalRange_2(t, h);
  }
  range<t_ind_0::iterator> equalRange_3(const t_tuple &t, context &h) const {
    auto pos = ind_0.find(t, h.hints_0);
    auto fin = ind_0.end();
    if (pos != fin) {
      fin = pos;
      ++fin;
    }
    return make_range(pos, fin);
  }
  range<t_ind_0::iterator> equalRange_3(const t_tuple &t) const {
    context h;
    return equalRange_3(t, h);
  }
  bool empty() const { return ind_0.empty(); }
  std::vector<range<iterator>> partition() const {
    return ind_0.getChunks(400);
  }
  void purge() { ind_0.clear(); }
  iterator begin() const { return ind_0.begin(); }
  iterator end() const { return ind_0.end(); }
  void printHintStatistics(std::ostream &o, const std::string prefix) const {
    const auto &stats_0 = ind_0.getHintStatistics();
    o << prefix << "arity 2 direct b-tree index [1,0]: (hits/misses/total)\n";
    o << prefix << "Insert: " << stats_0.inserts.getHits() << "/"
      << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses()
      << "\n";
    o << prefix << "Contains: " << stats_0.contains.getHits() << "/"
      << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses()
      << "\n";
    o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/"
      << stats_0.lower_bound.getMisses() << "/"
      << stats_0.lower_bound.getAccesses() << "\n";
    o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/"
      << stats_0.upper_bound.getMisses() << "/"
      << stats_0.upper_bound.getAccesses() << "\n";
  }
};

class Sf_ext_stg_liveness : public SouffleProgram {
private:
  static inline bool regex_wrapper(const std::string &pattern,
                                   const std::string &text) {
    bool result = false;
    try {
      result = std::regex_match(text, std::regex(pattern));
    } catch (...) {
      std::cerr << "warning: wrong pattern provided for match(\"" << pattern
                << "\",\"" << text << "\").\n";
    }
    return result;
  }

private:
  static inline std::string substr_wrapper(const std::string &str, size_t idx,
                                           size_t len) {
    std::string result;
    try {
      result = str.substr(idx, len);
    } catch (...) {
      std::cerr << "warning: wrong index position provided by substr(\"";
      std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len
                << ") functor.\n";
    }
    return result;
  }

private:
  static inline RamDomain wrapper_tonumber(const std::string &str) {
    RamDomain result = 0;
    try {
      result = stord(str);
    } catch (...) {
      std::cerr << "error: wrong string provided by to_number(\"";
      std::cerr << str << "\") functor.\n";
      raise(SIGFPE);
    }
    return result;
  }

public:
  // -- initialize symbol table --
  SymbolTable symTable; // -- Table: DataConReference
  std::unique_ptr<t_btree_2__0_1__1__3> rel_1_DataConReference =
      std::make_unique<t_btree_2__0_1__1__3>();
  souffle::RelationWrapper<0, t_btree_2__0_1__1__3, Tuple<RamDomain, 2>, 2, 1>
      wrapper_rel_1_DataConReference;
  // -- Table: FunReference
  std::unique_ptr<t_btree_2__0_1__1__3> rel_2_FunReference =
      std::make_unique<t_btree_2__0_1__1__3>();
  souffle::RelationWrapper<1, t_btree_2__0_1__1__3, Tuple<RamDomain, 2>, 2, 1>
      wrapper_rel_2_FunReference;
  // -- Table: LiveSource
  std::unique_ptr<t_btree_1__0__1> rel_3_LiveSource =
      std::make_unique<t_btree_1__0__1>();
  souffle::RelationWrapper<2, t_btree_1__0__1, Tuple<RamDomain, 1>, 1, 1>
      wrapper_rel_3_LiveSource;
  // -- Table: LiveFunName
  std::unique_ptr<t_btree_1__0__1> rel_4_LiveFunName =
      std::make_unique<t_btree_1__0__1>();
  souffle::RelationWrapper<3, t_btree_1__0__1, Tuple<RamDomain, 1>, 1, 1>
      wrapper_rel_4_LiveFunName;
  // -- Table: @delta_LiveFunName
  std::unique_ptr<t_btree_1__0__1> rel_5_delta_LiveFunName =
      std::make_unique<t_btree_1__0__1>();
  // -- Table: @new_LiveFunName
  std::unique_ptr<t_btree_1__0__1> rel_6_new_LiveFunName =
      std::make_unique<t_btree_1__0__1>();
  // -- Table: TyCon
  std::unique_ptr<t_btree_2__1_0__2__3> rel_7_TyCon =
      std::make_unique<t_btree_2__1_0__2__3>();
  souffle::RelationWrapper<4, t_btree_2__1_0__2__3, Tuple<RamDomain, 2>, 2, 1>
      wrapper_rel_7_TyCon;
  // -- Table: LiveDataConName
  std::unique_ptr<t_btree_1__0__1> rel_8_LiveDataConName =
      std::make_unique<t_btree_1__0__1>();
  souffle::RelationWrapper<5, t_btree_1__0__1, Tuple<RamDomain, 1>, 1, 1>
      wrapper_rel_8_LiveDataConName;
  // -- Table: TyConReference
  std::unique_ptr<t_btree_2__0_1__1__3> rel_9_TyConReference =
      std::make_unique<t_btree_2__0_1__1__3>();
  souffle::RelationWrapper<6, t_btree_2__0_1__1__3, Tuple<RamDomain, 2>, 2, 1>
      wrapper_rel_9_TyConReference;
  // -- Table: LiveTyConName
  std::unique_ptr<t_btree_1__0__1> rel_10_LiveTyConName =
      std::make_unique<t_btree_1__0__1>();
  souffle::RelationWrapper<7, t_btree_1__0__1, Tuple<RamDomain, 1>, 1, 1>
      wrapper_rel_10_LiveTyConName;

public:
  Sf_ext_stg_liveness()
      : wrapper_rel_1_DataConReference(
            *rel_1_DataConReference, symTable, "DataConReference",
            std::array<const char *, 2>{{"s:Name", "s:Name"}},
            std::array<const char *, 2>{{"fun", "datacon"}}),

        wrapper_rel_2_FunReference(
            *rel_2_FunReference, symTable, "FunReference",
            std::array<const char *, 2>{{"s:Name", "s:Name"}},
            std::array<const char *, 2>{{"fun", "funref"}}),

        wrapper_rel_3_LiveSource(*rel_3_LiveSource, symTable, "LiveSource",
                                 std::array<const char *, 1>{{"s:Name"}},
                                 std::array<const char *, 1>{{"fun"}}),

        wrapper_rel_4_LiveFunName(*rel_4_LiveFunName, symTable, "LiveFunName",
                                  std::array<const char *, 1>{{"s:Name"}},
                                  std::array<const char *, 1>{{"fun"}}),

        wrapper_rel_7_TyCon(*rel_7_TyCon, symTable, "TyCon",
                            std::array<const char *, 2>{{"s:Name", "s:Name"}},
                            std::array<const char *, 2>{{"tycon", "datacon"}}),

        wrapper_rel_8_LiveDataConName(*rel_8_LiveDataConName, symTable,
                                      "LiveDataConName",
                                      std::array<const char *, 1>{{"s:Name"}},
                                      std::array<const char *, 1>{{"datacon"}}),

        wrapper_rel_9_TyConReference(
            *rel_9_TyConReference, symTable, "TyConReference",
            std::array<const char *, 2>{{"s:Name", "s:Name"}},
            std::array<const char *, 2>{{"fun", "tycon"}}),

        wrapper_rel_10_LiveTyConName(*rel_10_LiveTyConName, symTable,
                                     "LiveTyConName",
                                     std::array<const char *, 1>{{"s:Name"}},
                                     std::array<const char *, 1>{{"tycon"}}) {
    addRelation("DataConReference", &wrapper_rel_1_DataConReference, true,
                false);
    addRelation("FunReference", &wrapper_rel_2_FunReference, true, false);
    addRelation("LiveSource", &wrapper_rel_3_LiveSource, true, false);
    addRelation("LiveFunName", &wrapper_rel_4_LiveFunName, false, true);
    addRelation("TyCon", &wrapper_rel_7_TyCon, true, false);
    addRelation("LiveDataConName", &wrapper_rel_8_LiveDataConName, false, true);
    addRelation("TyConReference", &wrapper_rel_9_TyConReference, true, false);
    addRelation("LiveTyConName", &wrapper_rel_10_LiveTyConName, false, true);
  }
  ~Sf_ext_stg_liveness() {}

private:
  void runFunction(std::string inputDirectory = ".",
                   std::string outputDirectory = ".",
                   size_t stratumIndex = (size_t)-1, bool performIO = false) {
    SignalHandler::instance()->set();
    std::atomic<size_t> iter(0);

#if defined(_OPENMP)
    if (getNumThreads() > 0) {
      omp_set_num_threads(getNumThreads());
    }
#endif

    // -- query evaluation --
    /* BEGIN STRATUM 0 */
    [&]() {
      if (performIO) {
        try {
          std::map<std::string, std::string> directiveMap(
              {{"IO", "file"},
               {"filename", "./DataConReference.facts"},
               {"name", "DataConReference"}});
          if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
              directiveMap["filename"].front() != '/') {
            directiveMap["filename"] =
                inputDirectory + "/" + directiveMap["filename"];
          }
          IODirectives ioDirectives(directiveMap);
          IOSystem::getInstance()
              .getReader(std::vector<bool>({1, 1}), symTable, ioDirectives,
                         false, 1)
              ->readAll(*rel_1_DataConReference);
        } catch (std::exception &e) {
          std::cerr << "Error loading data: " << e.what() << '\n';
        }
      }
    }();
    /* END STRATUM 0 */
    /* BEGIN STRATUM 1 */
    [&]() {
      if (performIO) {
        try {
          std::map<std::string, std::string> directiveMap(
              {{"IO", "file"},
               {"filename", "./FunReference.facts"},
               {"name", "FunReference"}});
          if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
              directiveMap["filename"].front() != '/') {
            directiveMap["filename"] =
                inputDirectory + "/" + directiveMap["filename"];
          }
          IODirectives ioDirectives(directiveMap);
          IOSystem::getInstance()
              .getReader(std::vector<bool>({1, 1}), symTable, ioDirectives,
                         false, 1)
              ->readAll(*rel_2_FunReference);
        } catch (std::exception &e) {
          std::cerr << "Error loading data: " << e.what() << '\n';
        }
      }
    }();
    /* END STRATUM 1 */
    /* BEGIN STRATUM 2 */
    [&]() {
      if (performIO) {
        try {
          std::map<std::string, std::string> directiveMap(
              {{"IO", "file"},
               {"filename", "./LiveSource.facts"},
               {"name", "LiveSource"}});
          if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
              directiveMap["filename"].front() != '/') {
            directiveMap["filename"] =
                inputDirectory + "/" + directiveMap["filename"];
          }
          IODirectives ioDirectives(directiveMap);
          IOSystem::getInstance()
              .getReader(std::vector<bool>({1}), symTable, ioDirectives, false,
                         1)
              ->readAll(*rel_3_LiveSource);
        } catch (std::exception &e) {
          std::cerr << "Error loading data: " << e.what() << '\n';
        }
      }
    }();
    /* END STRATUM 2 */
    /* BEGIN STRATUM 3 */
    [&]() {
      SignalHandler::instance()->setMsg(R"_(LiveFunName(fun) :- 
   LiveSource(fun).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [30:1-31:19])_");
      if (!(rel_3_LiveSource->empty())) {
        [&]() {
          CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,
                            rel_4_LiveFunName->createContext());
          CREATE_OP_CONTEXT(rel_3_LiveSource_op_ctxt,
                            rel_3_LiveSource->createContext());
          for (const auto &env0 : *rel_3_LiveSource) {
            Tuple<RamDomain, 1> tuple{{static_cast<RamDomain>(env0[0])}};
            rel_4_LiveFunName->insert(
                tuple, READ_OP_CONTEXT(rel_4_LiveFunName_op_ctxt));
          }
        }();
      }
      rel_5_delta_LiveFunName->insertAll(*rel_4_LiveFunName);
      iter = 0;
      for (;;) {
        SignalHandler::instance()->setMsg(R"_(LiveFunName(ref) :- 
   LiveFunName(fun),
   FunReference(fun,ref).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [33:1-35:26])_");
        if (!(rel_2_FunReference->empty()) &&
            !(rel_5_delta_LiveFunName->empty())) {
          [&]() {
            CREATE_OP_CONTEXT(rel_2_FunReference_op_ctxt,
                              rel_2_FunReference->createContext());
            CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,
                              rel_4_LiveFunName->createContext());
            CREATE_OP_CONTEXT(rel_6_new_LiveFunName_op_ctxt,
                              rel_6_new_LiveFunName->createContext());
            CREATE_OP_CONTEXT(rel_5_delta_LiveFunName_op_ctxt,
                              rel_5_delta_LiveFunName->createContext());
            for (const auto &env0 : *rel_5_delta_LiveFunName) {
              const Tuple<RamDomain, 2> key{{env0[0], 0}};
              auto range = rel_2_FunReference->equalRange_1(
                  key, READ_OP_CONTEXT(rel_2_FunReference_op_ctxt));
              for (const auto &env1 : range) {
                if (!(rel_4_LiveFunName->contains(
                        Tuple<RamDomain, 1>{{env1[1]}},
                        READ_OP_CONTEXT(rel_4_LiveFunName_op_ctxt)))) {
                  Tuple<RamDomain, 1> tuple{{static_cast<RamDomain>(env1[1])}};
                  rel_6_new_LiveFunName->insert(
                      tuple, READ_OP_CONTEXT(rel_6_new_LiveFunName_op_ctxt));
                }
              }
            }
          }();
        }
        if (rel_6_new_LiveFunName->empty())
          break;
        rel_4_LiveFunName->insertAll(*rel_6_new_LiveFunName);
        std::swap(rel_5_delta_LiveFunName, rel_6_new_LiveFunName);
        rel_6_new_LiveFunName->purge();
        iter++;
      }
      iter = 0;
      if (!isHintsProfilingEnabled())
        rel_5_delta_LiveFunName->purge();
      if (!isHintsProfilingEnabled())
        rel_6_new_LiveFunName->purge();
      if (performIO) {
        try {
          std::map<std::string, std::string> directiveMap(
              {{"IO", "file"},
               {"attributeNames", "fun"},
               {"filename", "./LiveFunName.csv"},
               {"name", "LiveFunName"}});
          if (!outputDirectory.empty() && directiveMap["IO"] == "file" &&
              directiveMap["filename"].front() != '/') {
            directiveMap["filename"] =
                outputDirectory + "/" + directiveMap["filename"];
          }
          IODirectives ioDirectives(directiveMap);
          IOSystem::getInstance()
              .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false,
                         1)
              ->writeAll(*rel_4_LiveFunName);
        } catch (std::exception &e) {
          std::cerr << e.what();
          exit(1);
        }
      }
      if (!isHintsProfilingEnabled() && performIO)
        rel_2_FunReference->purge();
    }();
    /* END STRATUM 3 */
    /* BEGIN STRATUM 4 */
    [&]() {
      if (performIO) {
        try {
          std::map<std::string, std::string> directiveMap(
              {{"IO", "file"},
               {"filename", "./TyCon.facts"},
               {"name", "TyCon"}});
          if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
              directiveMap["filename"].front() != '/') {
            directiveMap["filename"] =
                inputDirectory + "/" + directiveMap["filename"];
          }
          IODirectives ioDirectives(directiveMap);
          IOSystem::getInstance()
              .getReader(std::vector<bool>({1, 1}), symTable, ioDirectives,
                         false, 1)
              ->readAll(*rel_7_TyCon);
        } catch (std::exception &e) {
          std::cerr << "Error loading data: " << e.what() << '\n';
        }
      }
    }();
    /* END STRATUM 4 */
    /* BEGIN STRATUM 5 */
    [&]() {
      SignalHandler::instance()->setMsg(R"_(LiveDataConName(fun) :- 
   LiveSource(fun).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [38:1-39:19])_");
      if (!(rel_3_LiveSource->empty())) {
        [&]() {
          CREATE_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt,
                            rel_8_LiveDataConName->createContext());
          CREATE_OP_CONTEXT(rel_3_LiveSource_op_ctxt,
                            rel_3_LiveSource->createContext());
          for (const auto &env0 : *rel_3_LiveSource) {
            Tuple<RamDomain, 1> tuple{{static_cast<RamDomain>(env0[0])}};
            rel_8_LiveDataConName->insert(
                tuple, READ_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt));
          }
        }();
      }
      SignalHandler::instance()->setMsg(R"_(LiveDataConName(datacon) :- 
   LiveFunName(fun),
   DataConReference(fun,datacon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [41:1-43:34])_");
      if (!(rel_1_DataConReference->empty()) && !(rel_4_LiveFunName->empty())) {
        [&]() {
          CREATE_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt,
                            rel_8_LiveDataConName->createContext());
          CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,
                            rel_4_LiveFunName->createContext());
          CREATE_OP_CONTEXT(rel_1_DataConReference_op_ctxt,
                            rel_1_DataConReference->createContext());
          for (const auto &env0 : *rel_4_LiveFunName) {
            const Tuple<RamDomain, 2> key{{env0[0], 0}};
            auto range = rel_1_DataConReference->equalRange_1(
                key, READ_OP_CONTEXT(rel_1_DataConReference_op_ctxt));
            for (const auto &env1 : range) {
              Tuple<RamDomain, 1> tuple{{static_cast<RamDomain>(env1[1])}};
              rel_8_LiveDataConName->insert(
                  tuple, READ_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt));
            }
          }
        }();
      }
      SignalHandler::instance()->setMsg(R"_(LiveDataConName(datacon) :- 
   TyCon(_,datacon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [57:1-58:21])_");
      if (!(rel_7_TyCon->empty())) {
        [&]() {
          CREATE_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt,
                            rel_8_LiveDataConName->createContext());
          CREATE_OP_CONTEXT(rel_7_TyCon_op_ctxt, rel_7_TyCon->createContext());
          for (const auto &env0 : *rel_7_TyCon) {
            Tuple<RamDomain, 1> tuple{{static_cast<RamDomain>(env0[1])}};
            rel_8_LiveDataConName->insert(
                tuple, READ_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt));
          }
        }();
      }
      if (performIO) {
        try {
          std::map<std::string, std::string> directiveMap(
              {{"IO", "file"},
               {"attributeNames", "datacon"},
               {"filename", "./LiveDataConName.csv"},
               {"name", "LiveDataConName"}});
          if (!outputDirectory.empty() && directiveMap["IO"] == "file" &&
              directiveMap["filename"].front() != '/') {
            directiveMap["filename"] =
                outputDirectory + "/" + directiveMap["filename"];
          }
          IODirectives ioDirectives(directiveMap);
          IOSystem::getInstance()
              .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false,
                         1)
              ->writeAll(*rel_8_LiveDataConName);
        } catch (std::exception &e) {
          std::cerr << e.what();
          exit(1);
        }
      }
      if (!isHintsProfilingEnabled() && performIO)
        rel_1_DataConReference->purge();
      if (!isHintsProfilingEnabled() && performIO)
        rel_3_LiveSource->purge();
    }();
    /* END STRATUM 5 */
    /* BEGIN STRATUM 6 */
    [&]() {
      if (performIO) {
        try {
          std::map<std::string, std::string> directiveMap(
              {{"IO", "file"},
               {"filename", "./TyConReference.facts"},
               {"name", "TyConReference"}});
          if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
              directiveMap["filename"].front() != '/') {
            directiveMap["filename"] =
                inputDirectory + "/" + directiveMap["filename"];
          }
          IODirectives ioDirectives(directiveMap);
          IOSystem::getInstance()
              .getReader(std::vector<bool>({1, 1}), symTable, ioDirectives,
                         false, 1)
              ->readAll(*rel_9_TyConReference);
        } catch (std::exception &e) {
          std::cerr << "Error loading data: " << e.what() << '\n';
        }
      }
    }();
    /* END STRATUM 6 */
    /* BEGIN STRATUM 7 */
    [&]() {
      SignalHandler::instance()->setMsg(R"_(LiveTyConName(tycon) :- 
   LiveDataConName(datacon),
   TyCon(tycon,datacon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [46:1-48:25])_");
      if (!(rel_7_TyCon->empty()) && !(rel_8_LiveDataConName->empty())) {
        [&]() {
          CREATE_OP_CONTEXT(rel_8_LiveDataConName_op_ctxt,
                            rel_8_LiveDataConName->createContext());
          CREATE_OP_CONTEXT(rel_7_TyCon_op_ctxt, rel_7_TyCon->createContext());
          CREATE_OP_CONTEXT(rel_10_LiveTyConName_op_ctxt,
                            rel_10_LiveTyConName->createContext());
          for (const auto &env0 : *rel_8_LiveDataConName) {
            const Tuple<RamDomain, 2> key{{0, env0[0]}};
            auto range = rel_7_TyCon->equalRange_2(
                key, READ_OP_CONTEXT(rel_7_TyCon_op_ctxt));
            for (const auto &env1 : range) {
              Tuple<RamDomain, 1> tuple{{static_cast<RamDomain>(env1[0])}};
              rel_10_LiveTyConName->insert(
                  tuple, READ_OP_CONTEXT(rel_10_LiveTyConName_op_ctxt));
            }
          }
        }();
      }
      SignalHandler::instance()->setMsg(R"_(LiveTyConName(tycon) :- 
   LiveFunName(fun),
   TyConReference(fun,tycon).
in file /home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/external-stg-compiler/datalog/ext-stg-liveness.dl [50:1-52:30])_");
      if (!(rel_9_TyConReference->empty()) && !(rel_4_LiveFunName->empty())) {
        [&]() {
          CREATE_OP_CONTEXT(rel_4_LiveFunName_op_ctxt,
                            rel_4_LiveFunName->createContext());
          CREATE_OP_CONTEXT(rel_10_LiveTyConName_op_ctxt,
                            rel_10_LiveTyConName->createContext());
          CREATE_OP_CONTEXT(rel_9_TyConReference_op_ctxt,
                            rel_9_TyConReference->createContext());
          for (const auto &env0 : *rel_4_LiveFunName) {
            const Tuple<RamDomain, 2> key{{env0[0], 0}};
            auto range = rel_9_TyConReference->equalRange_1(
                key, READ_OP_CONTEXT(rel_9_TyConReference_op_ctxt));
            for (const auto &env1 : range) {
              Tuple<RamDomain, 1> tuple{{static_cast<RamDomain>(env1[1])}};
              rel_10_LiveTyConName->insert(
                  tuple, READ_OP_CONTEXT(rel_10_LiveTyConName_op_ctxt));
            }
          }
        }();
      }
      if (performIO) {
        try {
          std::map<std::string, std::string> directiveMap(
              {{"IO", "file"},
               {"attributeNames", "tycon"},
               {"filename", "./LiveTyConName.csv"},
               {"name", "LiveTyConName"}});
          if (!outputDirectory.empty() && directiveMap["IO"] == "file" &&
              directiveMap["filename"].front() != '/') {
            directiveMap["filename"] =
                outputDirectory + "/" + directiveMap["filename"];
          }
          IODirectives ioDirectives(directiveMap);
          IOSystem::getInstance()
              .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false,
                         1)
              ->writeAll(*rel_10_LiveTyConName);
        } catch (std::exception &e) {
          std::cerr << e.what();
          exit(1);
        }
      }
      if (!isHintsProfilingEnabled() && performIO)
        rel_7_TyCon->purge();
      if (!isHintsProfilingEnabled() && performIO)
        rel_9_TyConReference->purge();
      if (!isHintsProfilingEnabled() && performIO)
        rel_4_LiveFunName->purge();
      if (!isHintsProfilingEnabled() && performIO)
        rel_8_LiveDataConName->purge();
    }();
    /* END STRATUM 7 */

    // -- relation hint statistics --
    if (isHintsProfilingEnabled()) {
      std::cout << " -- Operation Hint Statistics --\n";
      std::cout << "Relation rel_1_DataConReference:\n";
      rel_1_DataConReference->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
      std::cout << "Relation rel_2_FunReference:\n";
      rel_2_FunReference->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
      std::cout << "Relation rel_3_LiveSource:\n";
      rel_3_LiveSource->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
      std::cout << "Relation rel_4_LiveFunName:\n";
      rel_4_LiveFunName->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
      std::cout << "Relation rel_5_delta_LiveFunName:\n";
      rel_5_delta_LiveFunName->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
      std::cout << "Relation rel_6_new_LiveFunName:\n";
      rel_6_new_LiveFunName->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
      std::cout << "Relation rel_7_TyCon:\n";
      rel_7_TyCon->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
      std::cout << "Relation rel_8_LiveDataConName:\n";
      rel_8_LiveDataConName->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
      std::cout << "Relation rel_9_TyConReference:\n";
      rel_9_TyConReference->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
      std::cout << "Relation rel_10_LiveTyConName:\n";
      rel_10_LiveTyConName->printHintStatistics(std::cout, "  ");
      std::cout << "\n";
    }
    SignalHandler::instance()->reset();
  }

public:
  void run(size_t stratumIndex = (size_t)-1) override {
    runFunction(".", ".", stratumIndex, false);
  }

public:
  void runAll(std::string inputDirectory = ".",
              std::string outputDirectory = ".",
              size_t stratumIndex = (size_t)-1) override {
    runFunction(inputDirectory, outputDirectory, stratumIndex, true);
  }

public:
  void printAll(std::string outputDirectory = ".") override {
    try {
      std::map<std::string, std::string> directiveMap(
          {{"IO", "file"},
           {"attributeNames", "fun"},
           {"filename", "./LiveFunName.csv"},
           {"name", "LiveFunName"}});
      if (!outputDirectory.empty() && directiveMap["IO"] == "file" &&
          directiveMap["filename"].front() != '/') {
        directiveMap["filename"] =
            outputDirectory + "/" + directiveMap["filename"];
      }
      IODirectives ioDirectives(directiveMap);
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false, 1)
          ->writeAll(*rel_4_LiveFunName);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
    try {
      std::map<std::string, std::string> directiveMap(
          {{"IO", "file"},
           {"attributeNames", "datacon"},
           {"filename", "./LiveDataConName.csv"},
           {"name", "LiveDataConName"}});
      if (!outputDirectory.empty() && directiveMap["IO"] == "file" &&
          directiveMap["filename"].front() != '/') {
        directiveMap["filename"] =
            outputDirectory + "/" + directiveMap["filename"];
      }
      IODirectives ioDirectives(directiveMap);
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false, 1)
          ->writeAll(*rel_8_LiveDataConName);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
    try {
      std::map<std::string, std::string> directiveMap(
          {{"IO", "file"},
           {"attributeNames", "tycon"},
           {"filename", "./LiveTyConName.csv"},
           {"name", "LiveTyConName"}});
      if (!outputDirectory.empty() && directiveMap["IO"] == "file" &&
          directiveMap["filename"].front() != '/') {
        directiveMap["filename"] =
            outputDirectory + "/" + directiveMap["filename"];
      }
      IODirectives ioDirectives(directiveMap);
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false, 1)
          ->writeAll(*rel_10_LiveTyConName);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
  }

public:
  void loadAll(std::string inputDirectory = ".") override {
    try {
      std::map<std::string, std::string> directiveMap(
          {{"IO", "file"},
           {"filename", "./DataConReference.facts"},
           {"name", "DataConReference"}});
      if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
          directiveMap["filename"].front() != '/') {
        directiveMap["filename"] =
            inputDirectory + "/" + directiveMap["filename"];
      }
      IODirectives ioDirectives(directiveMap);
      IOSystem::getInstance()
          .getReader(std::vector<bool>({1, 1}), symTable, ioDirectives, false,
                     1)
          ->readAll(*rel_1_DataConReference);
    } catch (std::exception &e) {
      std::cerr << "Error loading data: " << e.what() << '\n';
    }
    try {
      std::map<std::string, std::string> directiveMap(
          {{"IO", "file"},
           {"filename", "./FunReference.facts"},
           {"name", "FunReference"}});
      if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
          directiveMap["filename"].front() != '/') {
        directiveMap["filename"] =
            inputDirectory + "/" + directiveMap["filename"];
      }
      IODirectives ioDirectives(directiveMap);
      IOSystem::getInstance()
          .getReader(std::vector<bool>({1, 1}), symTable, ioDirectives, false,
                     1)
          ->readAll(*rel_2_FunReference);
    } catch (std::exception &e) {
      std::cerr << "Error loading data: " << e.what() << '\n';
    }
    try {
      std::map<std::string, std::string> directiveMap(
          {{"IO", "file"},
           {"filename", "./LiveSource.facts"},
           {"name", "LiveSource"}});
      if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
          directiveMap["filename"].front() != '/') {
        directiveMap["filename"] =
            inputDirectory + "/" + directiveMap["filename"];
      }
      IODirectives ioDirectives(directiveMap);
      IOSystem::getInstance()
          .getReader(std::vector<bool>({1}), symTable, ioDirectives, false, 1)
          ->readAll(*rel_3_LiveSource);
    } catch (std::exception &e) {
      std::cerr << "Error loading data: " << e.what() << '\n';
    }
    try {
      std::map<std::string, std::string> directiveMap(
          {{"IO", "file"}, {"filename", "./TyCon.facts"}, {"name", "TyCon"}});
      if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
          directiveMap["filename"].front() != '/') {
        directiveMap["filename"] =
            inputDirectory + "/" + directiveMap["filename"];
      }
      IODirectives ioDirectives(directiveMap);
      IOSystem::getInstance()
          .getReader(std::vector<bool>({1, 1}), symTable, ioDirectives, false,
                     1)
          ->readAll(*rel_7_TyCon);
    } catch (std::exception &e) {
      std::cerr << "Error loading data: " << e.what() << '\n';
    }
    try {
      std::map<std::string, std::string> directiveMap(
          {{"IO", "file"},
           {"filename", "./TyConReference.facts"},
           {"name", "TyConReference"}});
      if (!inputDirectory.empty() && directiveMap["IO"] == "file" &&
          directiveMap["filename"].front() != '/') {
        directiveMap["filename"] =
            inputDirectory + "/" + directiveMap["filename"];
      }
      IODirectives ioDirectives(directiveMap);
      IOSystem::getInstance()
          .getReader(std::vector<bool>({1, 1}), symTable, ioDirectives, false,
                     1)
          ->readAll(*rel_9_TyConReference);
    } catch (std::exception &e) {
      std::cerr << "Error loading data: " << e.what() << '\n';
    }
  }

public:
  void dumpInputs(std::ostream &out = std::cout) override {
    try {
      IODirectives ioDirectives;
      ioDirectives.setIOType("stdout");
      ioDirectives.setRelationName("rel_1_DataConReference");
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1, 1}), symTable, ioDirectives, false,
                     1)
          ->writeAll(*rel_1_DataConReference);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
    try {
      IODirectives ioDirectives;
      ioDirectives.setIOType("stdout");
      ioDirectives.setRelationName("rel_2_FunReference");
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1, 1}), symTable, ioDirectives, false,
                     1)
          ->writeAll(*rel_2_FunReference);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
    try {
      IODirectives ioDirectives;
      ioDirectives.setIOType("stdout");
      ioDirectives.setRelationName("rel_3_LiveSource");
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false, 1)
          ->writeAll(*rel_3_LiveSource);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
    try {
      IODirectives ioDirectives;
      ioDirectives.setIOType("stdout");
      ioDirectives.setRelationName("rel_7_TyCon");
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1, 1}), symTable, ioDirectives, false,
                     1)
          ->writeAll(*rel_7_TyCon);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
    try {
      IODirectives ioDirectives;
      ioDirectives.setIOType("stdout");
      ioDirectives.setRelationName("rel_9_TyConReference");
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1, 1}), symTable, ioDirectives, false,
                     1)
          ->writeAll(*rel_9_TyConReference);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
  }

public:
  void dumpOutputs(std::ostream &out = std::cout) override {
    try {
      IODirectives ioDirectives;
      ioDirectives.setIOType("stdout");
      ioDirectives.setRelationName("rel_4_LiveFunName");
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false, 1)
          ->writeAll(*rel_4_LiveFunName);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
    try {
      IODirectives ioDirectives;
      ioDirectives.setIOType("stdout");
      ioDirectives.setRelationName("rel_8_LiveDataConName");
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false, 1)
          ->writeAll(*rel_8_LiveDataConName);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
    try {
      IODirectives ioDirectives;
      ioDirectives.setIOType("stdout");
      ioDirectives.setRelationName("rel_10_LiveTyConName");
      IOSystem::getInstance()
          .getWriter(std::vector<bool>({1}), symTable, ioDirectives, false, 1)
          ->writeAll(*rel_10_LiveTyConName);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
  }

public:
  SymbolTable &getSymbolTable() override { return symTable; }
};
SouffleProgram *newInstance_ext_stg_liveness() {
  return new Sf_ext_stg_liveness;
}
SymbolTable *getST_ext_stg_liveness(SouffleProgram *p) {
  return &reinterpret_cast<Sf_ext_stg_liveness *>(p)->symTable;
}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_ext_stg_liveness : public souffle::ProgramFactory {
  SouffleProgram *newInstance() { return new Sf_ext_stg_liveness(); };

public:
  factory_Sf_ext_stg_liveness() : ProgramFactory("ext_stg_liveness") {}
};
static factory_Sf_ext_stg_liveness __factory_Sf_ext_stg_liveness_instance;
}
#else
}
int main(int argc, char **argv) {
  try {
    souffle::CmdOptions opt(R"(ext-stg-liveness.dl)", R"(.)", R"(.)", false,
                            R"()", 4, -1);
    if (!opt.parse(argc, argv))
      return 1;
    souffle::Sf_ext_stg_liveness obj;
#if defined(_OPENMP)
    obj.setNumThreads(opt.getNumJobs());

#endif
    obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir(),
               opt.getStratumIndex());
    return 0;
  } catch (std::exception &e) {
    souffle::SignalHandler::instance()->error(e.what());
  }
}

#endif
