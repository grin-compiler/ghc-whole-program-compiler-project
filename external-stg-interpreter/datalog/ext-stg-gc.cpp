#define SOUFFLE_GENERATOR_VERSION "2.4.1"
#include "souffle/CompiledSouffle.h"
#include "souffle/SignalHandler.h"
#include "souffle/SouffleInterface.h"
#include "souffle/datastructure/BTree.h"
#include "souffle/io/IOSystem.h"
#include "souffle/utility/MiscUtil.h"
#include <any>
namespace functors
{
    extern "C"
    {
    }
} // namespace functors
namespace souffle::t_btree_000_i__0__1
{
    using namespace souffle;
    struct Type
    {
        static constexpr Relation::arity_type Arity = 1;
        using t_tuple = Tuple<RamDomain, 1>;
        struct t_comparator_0
        {
            int operator()(const t_tuple &a, const t_tuple &b) const
            {
                return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1
                                                                                                                                                      : (0);
            }
            bool less(const t_tuple &a, const t_tuple &b) const
            {
                return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]));
            }
            bool equal(const t_tuple &a, const t_tuple &b) const
            {
                return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]));
            }
        };
        using t_ind_0 = btree_set<t_tuple, t_comparator_0>;
        t_ind_0 ind_0;
        using iterator = t_ind_0::iterator;
        struct context
        {
            t_ind_0::operation_hints hints_0_lower;
            t_ind_0::operation_hints hints_0_upper;
        };
        context createContext() { return context(); }
        bool insert(const t_tuple &t);
        bool insert(const t_tuple &t, context &h);
        bool insert(const RamDomain *ramDomain);
        bool insert(RamDomain a0);
        bool contains(const t_tuple &t, context &h) const;
        bool contains(const t_tuple &t) const;
        std::size_t size() const;
        iterator find(const t_tuple &t, context &h) const;
        iterator find(const t_tuple &t) const;
        range<iterator> lowerUpperRange_0(const t_tuple & /* lower */, const t_tuple & /* upper */, context & /* h */) const;
        range<iterator> lowerUpperRange_0(const t_tuple & /* lower */, const t_tuple & /* upper */) const;
        range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple &lower, const t_tuple &upper, context &h) const;
        range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple &lower, const t_tuple &upper) const;
        bool empty() const;
        std::vector<range<iterator>> partition() const;
        void purge();
        iterator begin() const;
        iterator end() const;
        void printStatistics(std::ostream &o) const;
    };
} // namespace souffle::t_btree_000_i__0__1
namespace souffle::t_btree_000_i__0__1
{
    using namespace souffle;
    using t_ind_0 = Type::t_ind_0;
    using iterator = Type::iterator;
    using context = Type::context;
    bool Type::insert(const t_tuple &t)
    {
        context h;
        return insert(t, h);
    }
    bool Type::insert(const t_tuple &t, context &h)
    {
        if (ind_0.insert(t, h.hints_0_lower))
        {
            return true;
        }
        else
            return false;
    }
    bool Type::insert(const RamDomain *ramDomain)
    {
        RamDomain data[1];
        std::copy(ramDomain, ramDomain + 1, data);
        const t_tuple &tuple = reinterpret_cast<const t_tuple &>(data);
        context h;
        return insert(tuple, h);
    }
    bool Type::insert(RamDomain a0)
    {
        RamDomain data[1] = {a0};
        return insert(data);
    }
    bool Type::contains(const t_tuple &t, context &h) const
    {
        return ind_0.contains(t, h.hints_0_lower);
    }
    bool Type::contains(const t_tuple &t) const
    {
        context h;
        return contains(t, h);
    }
    std::size_t Type::size() const
    {
        return ind_0.size();
    }
    iterator Type::find(const t_tuple &t, context &h) const
    {
        return ind_0.find(t, h.hints_0_lower);
    }
    iterator Type::find(const t_tuple &t) const
    {
        context h;
        return find(t, h);
    }
    range<iterator> Type::lowerUpperRange_0(const t_tuple & /* lower */, const t_tuple & /* upper */, context & /* h */) const
    {
        return range<iterator>(ind_0.begin(), ind_0.end());
    }
    range<iterator> Type::lowerUpperRange_0(const t_tuple & /* lower */, const t_tuple & /* upper */) const
    {
        return range<iterator>(ind_0.begin(), ind_0.end());
    }
    range<t_ind_0::iterator> Type::lowerUpperRange_1(const t_tuple &lower, const t_tuple &upper, context &h) const
    {
        t_comparator_0 comparator;
        int cmp = comparator(lower, upper);
        if (cmp == 0)
        {
            auto pos = ind_0.find(lower, h.hints_0_lower);
            auto fin = ind_0.end();
            if (pos != fin)
            {
                fin = pos;
                ++fin;
            }
            return make_range(pos, fin);
        }
        if (cmp > 0)
        {
            return make_range(ind_0.end(), ind_0.end());
        }
        return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
    }
    range<t_ind_0::iterator> Type::lowerUpperRange_1(const t_tuple &lower, const t_tuple &upper) const
    {
        context h;
        return lowerUpperRange_1(lower, upper, h);
    }
    bool Type::empty() const
    {
        return ind_0.empty();
    }
    std::vector<range<iterator>> Type::partition() const
    {
        return ind_0.getChunks(400);
    }
    void Type::purge()
    {
        ind_0.clear();
    }
    iterator Type::begin() const
    {
        return ind_0.begin();
    }
    iterator Type::end() const
    {
        return ind_0.end();
    }
    void Type::printStatistics(std::ostream &o) const
    {
        o << " arity 1 direct b-tree index 0 lex-order [0]\n";
        ind_0.printStats(o);
    }
} // namespace souffle::t_btree_000_i__0__1
namespace souffle::t_btree_000_ii__0_1__11__10
{
    using namespace souffle;
    struct Type
    {
        static constexpr Relation::arity_type Arity = 2;
        using t_tuple = Tuple<RamDomain, 2>;
        struct t_comparator_0
        {
            int operator()(const t_tuple &a, const t_tuple &b) const
            {
                return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1
                                                                                                                                                      : ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1
                                                                                                                                                                                                                                                                                        : (0));
            }
            bool less(const t_tuple &a, const t_tuple &b) const
            {
                return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) || ((ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))));
            }
            bool equal(const t_tuple &a, const t_tuple &b) const
            {
                return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
            }
        };
        using t_ind_0 = btree_set<t_tuple, t_comparator_0>;
        t_ind_0 ind_0;
        using iterator = t_ind_0::iterator;
        struct context
        {
            t_ind_0::operation_hints hints_0_lower;
            t_ind_0::operation_hints hints_0_upper;
        };
        context createContext() { return context(); }
        bool insert(const t_tuple &t);
        bool insert(const t_tuple &t, context &h);
        bool insert(const RamDomain *ramDomain);
        bool insert(RamDomain a0, RamDomain a1);
        bool contains(const t_tuple &t, context &h) const;
        bool contains(const t_tuple &t) const;
        std::size_t size() const;
        iterator find(const t_tuple &t, context &h) const;
        iterator find(const t_tuple &t) const;
        range<iterator> lowerUpperRange_00(const t_tuple & /* lower */, const t_tuple & /* upper */, context & /* h */) const;
        range<iterator> lowerUpperRange_00(const t_tuple & /* lower */, const t_tuple & /* upper */) const;
        range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple &lower, const t_tuple &upper, context &h) const;
        range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple &lower, const t_tuple &upper) const;
        range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple &lower, const t_tuple &upper, context &h) const;
        range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple &lower, const t_tuple &upper) const;
        bool empty() const;
        std::vector<range<iterator>> partition() const;
        void purge();
        iterator begin() const;
        iterator end() const;
        void printStatistics(std::ostream &o) const;
    };
} // namespace souffle::t_btree_000_ii__0_1__11__10
namespace souffle::t_btree_000_ii__0_1__11__10
{
    using namespace souffle;
    using t_ind_0 = Type::t_ind_0;
    using iterator = Type::iterator;
    using context = Type::context;
    bool Type::insert(const t_tuple &t)
    {
        context h;
        return insert(t, h);
    }
    bool Type::insert(const t_tuple &t, context &h)
    {
        if (ind_0.insert(t, h.hints_0_lower))
        {
            return true;
        }
        else
            return false;
    }
    bool Type::insert(const RamDomain *ramDomain)
    {
        RamDomain data[2];
        std::copy(ramDomain, ramDomain + 2, data);
        const t_tuple &tuple = reinterpret_cast<const t_tuple &>(data);
        context h;
        return insert(tuple, h);
    }
    bool Type::insert(RamDomain a0, RamDomain a1)
    {
        RamDomain data[2] = {a0, a1};
        return insert(data);
    }
    bool Type::contains(const t_tuple &t, context &h) const
    {
        return ind_0.contains(t, h.hints_0_lower);
    }
    bool Type::contains(const t_tuple &t) const
    {
        context h;
        return contains(t, h);
    }
    std::size_t Type::size() const
    {
        return ind_0.size();
    }
    iterator Type::find(const t_tuple &t, context &h) const
    {
        return ind_0.find(t, h.hints_0_lower);
    }
    iterator Type::find(const t_tuple &t) const
    {
        context h;
        return find(t, h);
    }
    range<iterator> Type::lowerUpperRange_00(const t_tuple & /* lower */, const t_tuple & /* upper */, context & /* h */) const
    {
        return range<iterator>(ind_0.begin(), ind_0.end());
    }
    range<iterator> Type::lowerUpperRange_00(const t_tuple & /* lower */, const t_tuple & /* upper */) const
    {
        return range<iterator>(ind_0.begin(), ind_0.end());
    }
    range<t_ind_0::iterator> Type::lowerUpperRange_11(const t_tuple &lower, const t_tuple &upper, context &h) const
    {
        t_comparator_0 comparator;
        int cmp = comparator(lower, upper);
        if (cmp == 0)
        {
            auto pos = ind_0.find(lower, h.hints_0_lower);
            auto fin = ind_0.end();
            if (pos != fin)
            {
                fin = pos;
                ++fin;
            }
            return make_range(pos, fin);
        }
        if (cmp > 0)
        {
            return make_range(ind_0.end(), ind_0.end());
        }
        return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
    }
    range<t_ind_0::iterator> Type::lowerUpperRange_11(const t_tuple &lower, const t_tuple &upper) const
    {
        context h;
        return lowerUpperRange_11(lower, upper, h);
    }
    range<t_ind_0::iterator> Type::lowerUpperRange_10(const t_tuple &lower, const t_tuple &upper, context &h) const
    {
        t_comparator_0 comparator;
        int cmp = comparator(lower, upper);
        if (cmp > 0)
        {
            return make_range(ind_0.end(), ind_0.end());
        }
        return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
    }
    range<t_ind_0::iterator> Type::lowerUpperRange_10(const t_tuple &lower, const t_tuple &upper) const
    {
        context h;
        return lowerUpperRange_10(lower, upper, h);
    }
    bool Type::empty() const
    {
        return ind_0.empty();
    }
    std::vector<range<iterator>> Type::partition() const
    {
        return ind_0.getChunks(400);
    }
    void Type::purge()
    {
        ind_0.clear();
    }
    iterator Type::begin() const
    {
        return ind_0.begin();
    }
    iterator Type::end() const
    {
        return ind_0.end();
    }
    void Type::printStatistics(std::ostream &o) const
    {
        o << " arity 2 direct b-tree index 0 lex-order [0,1]\n";
        ind_0.printStats(o);
    }
} // namespace souffle::t_btree_000_ii__0_1__11__10
namespace souffle
{
    using namespace souffle;
    class Stratum_DeadlockingThread_69894afebfc94aee
    {
    public:
        Stratum_DeadlockingThread_69894afebfc94aee(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_DeadlockingThread_8fa18e0d3ee84b8b, t_btree_000_i__0__1::Type &rel_LiveStep0_d54eeb7faabc4a13, t_btree_000_i__0__1::Type &rel_MaybeDeadlockingThread_bdb2bd10c95b5982);
        void run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret);

    private:
        SymbolTable &symTable;
        RecordTable &recordTable;
        ConcurrentCache<std::string, std::regex> &regexCache;
        bool &pruneImdtRels;
        bool &performIO;
        SignalHandler *&signalHandler;
        std::atomic<std::size_t> &iter;
        std::atomic<RamDomain> &ctr;
        std::string &inputDirectory;
        std::string &outputDirectory;
        t_btree_000_i__0__1::Type *rel_DeadlockingThread_8fa18e0d3ee84b8b;
        t_btree_000_i__0__1::Type *rel_LiveStep0_d54eeb7faabc4a13;
        t_btree_000_i__0__1::Type *rel_MaybeDeadlockingThread_bdb2bd10c95b5982;
    };
} // namespace  souffle
namespace souffle
{
    using namespace souffle;
    Stratum_DeadlockingThread_69894afebfc94aee::Stratum_DeadlockingThread_69894afebfc94aee(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_DeadlockingThread_8fa18e0d3ee84b8b, t_btree_000_i__0__1::Type &rel_LiveStep0_d54eeb7faabc4a13, t_btree_000_i__0__1::Type &rel_MaybeDeadlockingThread_bdb2bd10c95b5982) : symTable(symTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       recordTable(recordTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       regexCache(regexCache),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       pruneImdtRels(pruneImdtRels),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       performIO(performIO),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       signalHandler(signalHandler),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       iter(iter),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ctr(ctr),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       inputDirectory(inputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       outputDirectory(outputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       rel_DeadlockingThread_8fa18e0d3ee84b8b(&rel_DeadlockingThread_8fa18e0d3ee84b8b),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       rel_LiveStep0_d54eeb7faabc4a13(&rel_LiveStep0_d54eeb7faabc4a13),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       rel_MaybeDeadlockingThread_bdb2bd10c95b5982(&rel_MaybeDeadlockingThread_bdb2bd10c95b5982)
    {
    }

    void Stratum_DeadlockingThread_69894afebfc94aee::run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret)
    {
        signalHandler->setMsg(R"_(DeadlockingThread(tid) :- 
   MaybeDeadlockingThread(tid),
   !LiveStep0(tid).
in file ext-stg-gc.dl [32:1-34:19])_");
        if (!(rel_MaybeDeadlockingThread_bdb2bd10c95b5982->empty()))
        {
            [&]()
            {
                CREATE_OP_CONTEXT(rel_DeadlockingThread_8fa18e0d3ee84b8b_op_ctxt, rel_DeadlockingThread_8fa18e0d3ee84b8b->createContext());
                CREATE_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt, rel_LiveStep0_d54eeb7faabc4a13->createContext());
                CREATE_OP_CONTEXT(rel_MaybeDeadlockingThread_bdb2bd10c95b5982_op_ctxt, rel_MaybeDeadlockingThread_bdb2bd10c95b5982->createContext());
                for (const auto &env0 : *rel_MaybeDeadlockingThread_bdb2bd10c95b5982)
                {
                    if (!(rel_LiveStep0_d54eeb7faabc4a13->contains(Tuple<RamDomain, 1>{{ramBitCast(env0[0])}}, READ_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt))))
                    {
                        Tuple<RamDomain, 1> tuple{{ramBitCast(env0[0])}};
                        rel_DeadlockingThread_8fa18e0d3ee84b8b->insert(tuple, READ_OP_CONTEXT(rel_DeadlockingThread_8fa18e0d3ee84b8b_op_ctxt));
                    }
                }
            }();
        }
        if (performIO)
        {
            try
            {
                std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(threadId)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(name)_", R"_(DeadlockingThread)_"}, {R"_(operation)_", R"_(output)_"}, {R"_(output-dir)_", R"_(.)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 1, "params": ["threadId"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 1, "types": ["s:symbol"]}})_"}});
                if (outputDirectory == "-")
                {
                    directiveMap["IO"] = "stdout";
                    directiveMap["headers"] = "true";
                }
                else if (!outputDirectory.empty())
                {
                    directiveMap["output-dir"] = outputDirectory;
                }
                IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_DeadlockingThread_8fa18e0d3ee84b8b);
            }
            catch (std::exception &e)
            {
                std::cerr << e.what();
                exit(1);
            }
        }
        if (pruneImdtRels)
            rel_MaybeDeadlockingThread_bdb2bd10c95b5982->purge();
    }

} // namespace  souffle

namespace souffle
{
    using namespace souffle;
    class Stratum_GCRoot_b08a674c48c5fe0e
    {
    public:
        Stratum_GCRoot_b08a674c48c5fe0e(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_GCRoot_f9754bdf5b76c5df);
        void run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret);

    private:
        SymbolTable &symTable;
        RecordTable &recordTable;
        ConcurrentCache<std::string, std::regex> &regexCache;
        bool &pruneImdtRels;
        bool &performIO;
        SignalHandler *&signalHandler;
        std::atomic<std::size_t> &iter;
        std::atomic<RamDomain> &ctr;
        std::string &inputDirectory;
        std::string &outputDirectory;
        t_btree_000_i__0__1::Type *rel_GCRoot_f9754bdf5b76c5df;
    };
} // namespace  souffle
namespace souffle
{
    using namespace souffle;
    Stratum_GCRoot_b08a674c48c5fe0e::Stratum_GCRoot_b08a674c48c5fe0e(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_GCRoot_f9754bdf5b76c5df) : symTable(symTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                   recordTable(recordTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                   regexCache(regexCache),
                                                                                                                                                                                                                                                                                                                                                                                                                                   pruneImdtRels(pruneImdtRels),
                                                                                                                                                                                                                                                                                                                                                                                                                                   performIO(performIO),
                                                                                                                                                                                                                                                                                                                                                                                                                                   signalHandler(signalHandler),
                                                                                                                                                                                                                                                                                                                                                                                                                                   iter(iter),
                                                                                                                                                                                                                                                                                                                                                                                                                                   ctr(ctr),
                                                                                                                                                                                                                                                                                                                                                                                                                                   inputDirectory(inputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                   outputDirectory(outputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                   rel_GCRoot_f9754bdf5b76c5df(&rel_GCRoot_f9754bdf5b76c5df)
    {
    }

    void Stratum_GCRoot_b08a674c48c5fe0e::run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret)
    {
        if (performIO)
        {
            try
            {
                std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(val)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(fact-dir)_", R"_(.)_"}, {R"_(name)_", R"_(GCRoot)_"}, {R"_(operation)_", R"_(input)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 1, "params": ["val"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 1, "types": ["s:symbol"]}})_"}});
                if (!inputDirectory.empty())
                {
                    directiveMap["fact-dir"] = inputDirectory;
                }
                IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_GCRoot_f9754bdf5b76c5df);
            }
            catch (std::exception &e)
            {
                std::cerr << "Error loading GCRoot data: " << e.what() << '\n';
                exit(1);
            }
        }
    }

} // namespace  souffle

namespace souffle
{
    using namespace souffle;
    class Stratum_Live_b9069971975f423e
    {
    public:
        Stratum_Live_b9069971975f423e(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_delta_Live_2c57e9662e50a2a0, t_btree_000_i__0__1::Type &rel_new_Live_ca472dbac4201e48, t_btree_000_i__0__1::Type &rel_DeadlockingThread_8fa18e0d3ee84b8b, t_btree_000_i__0__1::Type &rel_Live_2818460375647c67, t_btree_000_i__0__1::Type &rel_LiveStep0_d54eeb7faabc4a13, t_btree_000_ii__0_1__11__10::Type &rel_Reference_c57e388e43703de5);
        void run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret);

    private:
        SymbolTable &symTable;
        RecordTable &recordTable;
        ConcurrentCache<std::string, std::regex> &regexCache;
        bool &pruneImdtRels;
        bool &performIO;
        SignalHandler *&signalHandler;
        std::atomic<std::size_t> &iter;
        std::atomic<RamDomain> &ctr;
        std::string &inputDirectory;
        std::string &outputDirectory;
        t_btree_000_i__0__1::Type *rel_delta_Live_2c57e9662e50a2a0;
        t_btree_000_i__0__1::Type *rel_new_Live_ca472dbac4201e48;
        t_btree_000_i__0__1::Type *rel_DeadlockingThread_8fa18e0d3ee84b8b;
        t_btree_000_i__0__1::Type *rel_Live_2818460375647c67;
        t_btree_000_i__0__1::Type *rel_LiveStep0_d54eeb7faabc4a13;
        t_btree_000_ii__0_1__11__10::Type *rel_Reference_c57e388e43703de5;
    };
} // namespace  souffle
namespace souffle
{
    using namespace souffle;
    Stratum_Live_b9069971975f423e::Stratum_Live_b9069971975f423e(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_delta_Live_2c57e9662e50a2a0, t_btree_000_i__0__1::Type &rel_new_Live_ca472dbac4201e48, t_btree_000_i__0__1::Type &rel_DeadlockingThread_8fa18e0d3ee84b8b, t_btree_000_i__0__1::Type &rel_Live_2818460375647c67, t_btree_000_i__0__1::Type &rel_LiveStep0_d54eeb7faabc4a13, t_btree_000_ii__0_1__11__10::Type &rel_Reference_c57e388e43703de5) : symTable(symTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    recordTable(recordTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    regexCache(regexCache),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    pruneImdtRels(pruneImdtRels),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    performIO(performIO),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    signalHandler(signalHandler),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    iter(iter),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ctr(ctr),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    inputDirectory(inputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    outputDirectory(outputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    rel_delta_Live_2c57e9662e50a2a0(&rel_delta_Live_2c57e9662e50a2a0),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    rel_new_Live_ca472dbac4201e48(&rel_new_Live_ca472dbac4201e48),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    rel_DeadlockingThread_8fa18e0d3ee84b8b(&rel_DeadlockingThread_8fa18e0d3ee84b8b),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    rel_Live_2818460375647c67(&rel_Live_2818460375647c67),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    rel_LiveStep0_d54eeb7faabc4a13(&rel_LiveStep0_d54eeb7faabc4a13),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    rel_Reference_c57e388e43703de5(&rel_Reference_c57e388e43703de5)
    {
    }

    void Stratum_Live_b9069971975f423e::run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret)
    {
        signalHandler->setMsg(R"_(Live(tid) :- 
   DeadlockingThread(tid).
in file ext-stg-gc.dl [36:1-36:37])_");
        if (!(rel_DeadlockingThread_8fa18e0d3ee84b8b->empty()))
        {
            [&]()
            {
                CREATE_OP_CONTEXT(rel_DeadlockingThread_8fa18e0d3ee84b8b_op_ctxt, rel_DeadlockingThread_8fa18e0d3ee84b8b->createContext());
                CREATE_OP_CONTEXT(rel_Live_2818460375647c67_op_ctxt, rel_Live_2818460375647c67->createContext());
                for (const auto &env0 : *rel_DeadlockingThread_8fa18e0d3ee84b8b)
                {
                    Tuple<RamDomain, 1> tuple{{ramBitCast(env0[0])}};
                    rel_Live_2818460375647c67->insert(tuple, READ_OP_CONTEXT(rel_Live_2818460375647c67_op_ctxt));
                }
            }();
        }
        signalHandler->setMsg(R"_(Live(ref) :- 
   LiveStep0(ref).
in file ext-stg-gc.dl [37:1-37:29])_");
        if (!(rel_LiveStep0_d54eeb7faabc4a13->empty()))
        {
            [&]()
            {
                CREATE_OP_CONTEXT(rel_Live_2818460375647c67_op_ctxt, rel_Live_2818460375647c67->createContext());
                CREATE_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt, rel_LiveStep0_d54eeb7faabc4a13->createContext());
                for (const auto &env0 : *rel_LiveStep0_d54eeb7faabc4a13)
                {
                    Tuple<RamDomain, 1> tuple{{ramBitCast(env0[0])}};
                    rel_Live_2818460375647c67->insert(tuple, READ_OP_CONTEXT(rel_Live_2818460375647c67_op_ctxt));
                }
            }();
        }
        [&]()
        {
            CREATE_OP_CONTEXT(rel_delta_Live_2c57e9662e50a2a0_op_ctxt, rel_delta_Live_2c57e9662e50a2a0->createContext());
            CREATE_OP_CONTEXT(rel_Live_2818460375647c67_op_ctxt, rel_Live_2818460375647c67->createContext());
            for (const auto &env0 : *rel_Live_2818460375647c67)
            {
                Tuple<RamDomain, 1> tuple{{ramBitCast(env0[0])}};
                rel_delta_Live_2c57e9662e50a2a0->insert(tuple, READ_OP_CONTEXT(rel_delta_Live_2c57e9662e50a2a0_op_ctxt));
            }
        }();
        auto loop_counter = RamUnsigned(1);
        iter = 0;
        for (;;)
        {
            signalHandler->setMsg(R"_(Live(to) :- 
   Live(from),
   Reference(from,to).
in file ext-stg-gc.dl [38:1-40:23])_");
            if (!(rel_delta_Live_2c57e9662e50a2a0->empty()) && !(rel_Reference_c57e388e43703de5->empty()))
            {
                [&]()
                {
                    CREATE_OP_CONTEXT(rel_delta_Live_2c57e9662e50a2a0_op_ctxt, rel_delta_Live_2c57e9662e50a2a0->createContext());
                    CREATE_OP_CONTEXT(rel_new_Live_ca472dbac4201e48_op_ctxt, rel_new_Live_ca472dbac4201e48->createContext());
                    CREATE_OP_CONTEXT(rel_Live_2818460375647c67_op_ctxt, rel_Live_2818460375647c67->createContext());
                    CREATE_OP_CONTEXT(rel_Reference_c57e388e43703de5_op_ctxt, rel_Reference_c57e388e43703de5->createContext());
                    for (const auto &env0 : *rel_delta_Live_2c57e9662e50a2a0)
                    {
                        auto range = rel_Reference_c57e388e43703de5->lowerUpperRange_10(Tuple<RamDomain, 2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}}, Tuple<RamDomain, 2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}}, READ_OP_CONTEXT(rel_Reference_c57e388e43703de5_op_ctxt));
                        for (const auto &env1 : range)
                        {
                            if (!(rel_Live_2818460375647c67->contains(Tuple<RamDomain, 1>{{ramBitCast(env1[1])}}, READ_OP_CONTEXT(rel_Live_2818460375647c67_op_ctxt))))
                            {
                                Tuple<RamDomain, 1> tuple{{ramBitCast(env1[1])}};
                                rel_new_Live_ca472dbac4201e48->insert(tuple, READ_OP_CONTEXT(rel_new_Live_ca472dbac4201e48_op_ctxt));
                            }
                        }
                    }
                }();
            }
            if (rel_new_Live_ca472dbac4201e48->empty())
                break;
            [&]()
            {
                CREATE_OP_CONTEXT(rel_new_Live_ca472dbac4201e48_op_ctxt, rel_new_Live_ca472dbac4201e48->createContext());
                CREATE_OP_CONTEXT(rel_Live_2818460375647c67_op_ctxt, rel_Live_2818460375647c67->createContext());
                for (const auto &env0 : *rel_new_Live_ca472dbac4201e48)
                {
                    Tuple<RamDomain, 1> tuple{{ramBitCast(env0[0])}};
                    rel_Live_2818460375647c67->insert(tuple, READ_OP_CONTEXT(rel_Live_2818460375647c67_op_ctxt));
                }
            }();
            std::swap(rel_delta_Live_2c57e9662e50a2a0, rel_new_Live_ca472dbac4201e48);
            rel_new_Live_ca472dbac4201e48->purge();
            loop_counter = (ramBitCast<RamUnsigned>(loop_counter) + ramBitCast<RamUnsigned>(RamUnsigned(1)));
            iter++;
        }
        iter = 0;
        rel_delta_Live_2c57e9662e50a2a0->purge();
        rel_new_Live_ca472dbac4201e48->purge();
        if (performIO)
        {
            try
            {
                std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(val)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(name)_", R"_(Live)_"}, {R"_(operation)_", R"_(output)_"}, {R"_(output-dir)_", R"_(.)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 1, "params": ["val"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 1, "types": ["s:symbol"]}})_"}});
                if (outputDirectory == "-")
                {
                    directiveMap["IO"] = "stdout";
                    directiveMap["headers"] = "true";
                }
                else if (!outputDirectory.empty())
                {
                    directiveMap["output-dir"] = outputDirectory;
                }
                IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_Live_2818460375647c67);
            }
            catch (std::exception &e)
            {
                std::cerr << e.what();
                exit(1);
            }
        }
        if (pruneImdtRels)
            rel_LiveStep0_d54eeb7faabc4a13->purge();
        if (pruneImdtRels)
            rel_Reference_c57e388e43703de5->purge();
    }

} // namespace  souffle

namespace souffle
{
    using namespace souffle;
    class Stratum_LiveStep0_41822abe1018780c
    {
    public:
        Stratum_LiveStep0_41822abe1018780c(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_delta_LiveStep0_749758939b4491e5, t_btree_000_i__0__1::Type &rel_new_LiveStep0_b9970262b9a948e9, t_btree_000_i__0__1::Type &rel_GCRoot_f9754bdf5b76c5df, t_btree_000_i__0__1::Type &rel_LiveStep0_d54eeb7faabc4a13, t_btree_000_ii__0_1__11__10::Type &rel_Reference_c57e388e43703de5);
        void run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret);

    private:
        SymbolTable &symTable;
        RecordTable &recordTable;
        ConcurrentCache<std::string, std::regex> &regexCache;
        bool &pruneImdtRels;
        bool &performIO;
        SignalHandler *&signalHandler;
        std::atomic<std::size_t> &iter;
        std::atomic<RamDomain> &ctr;
        std::string &inputDirectory;
        std::string &outputDirectory;
        t_btree_000_i__0__1::Type *rel_delta_LiveStep0_749758939b4491e5;
        t_btree_000_i__0__1::Type *rel_new_LiveStep0_b9970262b9a948e9;
        t_btree_000_i__0__1::Type *rel_GCRoot_f9754bdf5b76c5df;
        t_btree_000_i__0__1::Type *rel_LiveStep0_d54eeb7faabc4a13;
        t_btree_000_ii__0_1__11__10::Type *rel_Reference_c57e388e43703de5;
    };
} // namespace  souffle
namespace souffle
{
    using namespace souffle;
    Stratum_LiveStep0_41822abe1018780c::Stratum_LiveStep0_41822abe1018780c(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_delta_LiveStep0_749758939b4491e5, t_btree_000_i__0__1::Type &rel_new_LiveStep0_b9970262b9a948e9, t_btree_000_i__0__1::Type &rel_GCRoot_f9754bdf5b76c5df, t_btree_000_i__0__1::Type &rel_LiveStep0_d54eeb7faabc4a13, t_btree_000_ii__0_1__11__10::Type &rel_Reference_c57e388e43703de5) : symTable(symTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       recordTable(recordTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       regexCache(regexCache),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       pruneImdtRels(pruneImdtRels),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       performIO(performIO),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       signalHandler(signalHandler),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       iter(iter),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ctr(ctr),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       inputDirectory(inputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       outputDirectory(outputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       rel_delta_LiveStep0_749758939b4491e5(&rel_delta_LiveStep0_749758939b4491e5),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       rel_new_LiveStep0_b9970262b9a948e9(&rel_new_LiveStep0_b9970262b9a948e9),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       rel_GCRoot_f9754bdf5b76c5df(&rel_GCRoot_f9754bdf5b76c5df),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       rel_LiveStep0_d54eeb7faabc4a13(&rel_LiveStep0_d54eeb7faabc4a13),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       rel_Reference_c57e388e43703de5(&rel_Reference_c57e388e43703de5)
    {
    }

    void Stratum_LiveStep0_41822abe1018780c::run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret)
    {
        signalHandler->setMsg(R"_(LiveStep0(ref) :- 
   GCRoot(ref).
in file ext-stg-gc.dl [18:1-18:31])_");
        if (!(rel_GCRoot_f9754bdf5b76c5df->empty()))
        {
            [&]()
            {
                CREATE_OP_CONTEXT(rel_GCRoot_f9754bdf5b76c5df_op_ctxt, rel_GCRoot_f9754bdf5b76c5df->createContext());
                CREATE_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt, rel_LiveStep0_d54eeb7faabc4a13->createContext());
                for (const auto &env0 : *rel_GCRoot_f9754bdf5b76c5df)
                {
                    Tuple<RamDomain, 1> tuple{{ramBitCast(env0[0])}};
                    rel_LiveStep0_d54eeb7faabc4a13->insert(tuple, READ_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt));
                }
            }();
        }
        [&]()
        {
            CREATE_OP_CONTEXT(rel_delta_LiveStep0_749758939b4491e5_op_ctxt, rel_delta_LiveStep0_749758939b4491e5->createContext());
            CREATE_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt, rel_LiveStep0_d54eeb7faabc4a13->createContext());
            for (const auto &env0 : *rel_LiveStep0_d54eeb7faabc4a13)
            {
                Tuple<RamDomain, 1> tuple{{ramBitCast(env0[0])}};
                rel_delta_LiveStep0_749758939b4491e5->insert(tuple, READ_OP_CONTEXT(rel_delta_LiveStep0_749758939b4491e5_op_ctxt));
            }
        }();
        auto loop_counter = RamUnsigned(1);
        iter = 0;
        for (;;)
        {
            signalHandler->setMsg(R"_(LiveStep0(to) :- 
   LiveStep0(from),
   Reference(from,to).
in file ext-stg-gc.dl [20:1-22:23])_");
            if (!(rel_delta_LiveStep0_749758939b4491e5->empty()) && !(rel_Reference_c57e388e43703de5->empty()))
            {
                [&]()
                {
                    CREATE_OP_CONTEXT(rel_delta_LiveStep0_749758939b4491e5_op_ctxt, rel_delta_LiveStep0_749758939b4491e5->createContext());
                    CREATE_OP_CONTEXT(rel_new_LiveStep0_b9970262b9a948e9_op_ctxt, rel_new_LiveStep0_b9970262b9a948e9->createContext());
                    CREATE_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt, rel_LiveStep0_d54eeb7faabc4a13->createContext());
                    CREATE_OP_CONTEXT(rel_Reference_c57e388e43703de5_op_ctxt, rel_Reference_c57e388e43703de5->createContext());
                    for (const auto &env0 : *rel_delta_LiveStep0_749758939b4491e5)
                    {
                        auto range = rel_Reference_c57e388e43703de5->lowerUpperRange_10(Tuple<RamDomain, 2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}}, Tuple<RamDomain, 2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}}, READ_OP_CONTEXT(rel_Reference_c57e388e43703de5_op_ctxt));
                        for (const auto &env1 : range)
                        {
                            if (!(rel_LiveStep0_d54eeb7faabc4a13->contains(Tuple<RamDomain, 1>{{ramBitCast(env1[1])}}, READ_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt))))
                            {
                                Tuple<RamDomain, 1> tuple{{ramBitCast(env1[1])}};
                                rel_new_LiveStep0_b9970262b9a948e9->insert(tuple, READ_OP_CONTEXT(rel_new_LiveStep0_b9970262b9a948e9_op_ctxt));
                            }
                        }
                    }
                }();
            }
            if (rel_new_LiveStep0_b9970262b9a948e9->empty())
                break;
            [&]()
            {
                CREATE_OP_CONTEXT(rel_new_LiveStep0_b9970262b9a948e9_op_ctxt, rel_new_LiveStep0_b9970262b9a948e9->createContext());
                CREATE_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt, rel_LiveStep0_d54eeb7faabc4a13->createContext());
                for (const auto &env0 : *rel_new_LiveStep0_b9970262b9a948e9)
                {
                    Tuple<RamDomain, 1> tuple{{ramBitCast(env0[0])}};
                    rel_LiveStep0_d54eeb7faabc4a13->insert(tuple, READ_OP_CONTEXT(rel_LiveStep0_d54eeb7faabc4a13_op_ctxt));
                }
            }();
            std::swap(rel_delta_LiveStep0_749758939b4491e5, rel_new_LiveStep0_b9970262b9a948e9);
            rel_new_LiveStep0_b9970262b9a948e9->purge();
            loop_counter = (ramBitCast<RamUnsigned>(loop_counter) + ramBitCast<RamUnsigned>(RamUnsigned(1)));
            iter++;
        }
        iter = 0;
        rel_delta_LiveStep0_749758939b4491e5->purge();
        rel_new_LiveStep0_b9970262b9a948e9->purge();
        if (pruneImdtRels)
            rel_GCRoot_f9754bdf5b76c5df->purge();
    }

} // namespace  souffle

namespace souffle
{
    using namespace souffle;
    class Stratum_MaybeDeadlockingThread_fd0c8181097ea422
    {
    public:
        Stratum_MaybeDeadlockingThread_fd0c8181097ea422(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_MaybeDeadlockingThread_bdb2bd10c95b5982);
        void run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret);

    private:
        SymbolTable &symTable;
        RecordTable &recordTable;
        ConcurrentCache<std::string, std::regex> &regexCache;
        bool &pruneImdtRels;
        bool &performIO;
        SignalHandler *&signalHandler;
        std::atomic<std::size_t> &iter;
        std::atomic<RamDomain> &ctr;
        std::string &inputDirectory;
        std::string &outputDirectory;
        t_btree_000_i__0__1::Type *rel_MaybeDeadlockingThread_bdb2bd10c95b5982;
    };
} // namespace  souffle
namespace souffle
{
    using namespace souffle;
    Stratum_MaybeDeadlockingThread_fd0c8181097ea422::Stratum_MaybeDeadlockingThread_fd0c8181097ea422(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_i__0__1::Type &rel_MaybeDeadlockingThread_bdb2bd10c95b5982) : symTable(symTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   recordTable(recordTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   regexCache(regexCache),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   pruneImdtRels(pruneImdtRels),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   performIO(performIO),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   signalHandler(signalHandler),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   iter(iter),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ctr(ctr),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   inputDirectory(inputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   outputDirectory(outputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   rel_MaybeDeadlockingThread_bdb2bd10c95b5982(&rel_MaybeDeadlockingThread_bdb2bd10c95b5982)
    {
    }

    void Stratum_MaybeDeadlockingThread_fd0c8181097ea422::run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret)
    {
        if (performIO)
        {
            try
            {
                std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(threadId)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(fact-dir)_", R"_(.)_"}, {R"_(name)_", R"_(MaybeDeadlockingThread)_"}, {R"_(operation)_", R"_(input)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 1, "params": ["threadId"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 1, "types": ["s:symbol"]}})_"}});
                if (!inputDirectory.empty())
                {
                    directiveMap["fact-dir"] = inputDirectory;
                }
                IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_MaybeDeadlockingThread_bdb2bd10c95b5982);
            }
            catch (std::exception &e)
            {
                std::cerr << "Error loading MaybeDeadlockingThread data: " << e.what() << '\n';
                exit(1);
            }
        }
    }

} // namespace  souffle

namespace souffle
{
    using namespace souffle;
    class Stratum_Reference_374a4e7377ff135c
    {
    public:
        Stratum_Reference_374a4e7377ff135c(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_ii__0_1__11__10::Type &rel_Reference_c57e388e43703de5);
        void run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret);

    private:
        SymbolTable &symTable;
        RecordTable &recordTable;
        ConcurrentCache<std::string, std::regex> &regexCache;
        bool &pruneImdtRels;
        bool &performIO;
        SignalHandler *&signalHandler;
        std::atomic<std::size_t> &iter;
        std::atomic<RamDomain> &ctr;
        std::string &inputDirectory;
        std::string &outputDirectory;
        t_btree_000_ii__0_1__11__10::Type *rel_Reference_c57e388e43703de5;
    };
} // namespace  souffle
namespace souffle
{
    using namespace souffle;
    Stratum_Reference_374a4e7377ff135c::Stratum_Reference_374a4e7377ff135c(SymbolTable &symTable, RecordTable &recordTable, ConcurrentCache<std::string, std::regex> &regexCache, bool &pruneImdtRels, bool &performIO, SignalHandler *&signalHandler, std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr, std::string &inputDirectory, std::string &outputDirectory, t_btree_000_ii__0_1__11__10::Type &rel_Reference_c57e388e43703de5) : symTable(symTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    recordTable(recordTable),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    regexCache(regexCache),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    pruneImdtRels(pruneImdtRels),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    performIO(performIO),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    signalHandler(signalHandler),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    iter(iter),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    ctr(ctr),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    inputDirectory(inputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    outputDirectory(outputDirectory),
                                                                                                                                                                                                                                                                                                                                                                                                                                                    rel_Reference_c57e388e43703de5(&rel_Reference_c57e388e43703de5)
    {
    }

    void Stratum_Reference_374a4e7377ff135c::run([[maybe_unused]] const std::vector<RamDomain> &args, [[maybe_unused]] std::vector<RamDomain> &ret)
    {
        if (performIO)
        {
            try
            {
                std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(from	to)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(fact-dir)_", R"_(.)_"}, {R"_(name)_", R"_(Reference)_"}, {R"_(operation)_", R"_(input)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 2, "params": ["from", "to"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 2, "types": ["s:symbol", "s:symbol"]}})_"}});
                if (!inputDirectory.empty())
                {
                    directiveMap["fact-dir"] = inputDirectory;
                }
                IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_Reference_c57e388e43703de5);
            }
            catch (std::exception &e)
            {
                std::cerr << "Error loading Reference data: " << e.what() << '\n';
                exit(1);
            }
        }
    }

} // namespace  souffle

namespace souffle
{
    using namespace souffle;
    class Sf_ext_stg_gc : public SouffleProgram
    {
    public:
        Sf_ext_stg_gc();
        ~Sf_ext_stg_gc();
        void run();
        void runAll(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "", bool performIOArg = true, bool pruneImdtRelsArg = true);
        void printAll([[maybe_unused]] std::string outputDirectoryArg = "");
        void loadAll([[maybe_unused]] std::string inputDirectoryArg = "");
        void dumpInputs();
        void dumpOutputs();
        SymbolTable &getSymbolTable();
        RecordTable &getRecordTable();
        void setNumThreads(std::size_t numThreadsValue);
        void executeSubroutine(std::string name, const std::vector<RamDomain> &args, std::vector<RamDomain> &ret);

    private:
        void runFunction(std::string inputDirectoryArg, std::string outputDirectoryArg, bool performIOArg, bool pruneImdtRelsArg);
        SymbolTableImpl symTable;
        SpecializedRecordTable<0> recordTable;
        ConcurrentCache<std::string, std::regex> regexCache;
        Own<t_btree_000_i__0__1::Type> rel_GCRoot_f9754bdf5b76c5df;
        souffle::RelationWrapper<t_btree_000_i__0__1::Type> wrapper_rel_GCRoot_f9754bdf5b76c5df;
        Own<t_btree_000_i__0__1::Type> rel_MaybeDeadlockingThread_bdb2bd10c95b5982;
        souffle::RelationWrapper<t_btree_000_i__0__1::Type> wrapper_rel_MaybeDeadlockingThread_bdb2bd10c95b5982;
        Own<t_btree_000_ii__0_1__11__10::Type> rel_Reference_c57e388e43703de5;
        souffle::RelationWrapper<t_btree_000_ii__0_1__11__10::Type> wrapper_rel_Reference_c57e388e43703de5;
        Own<t_btree_000_i__0__1::Type> rel_LiveStep0_d54eeb7faabc4a13;
        souffle::RelationWrapper<t_btree_000_i__0__1::Type> wrapper_rel_LiveStep0_d54eeb7faabc4a13;
        Own<t_btree_000_i__0__1::Type> rel_new_LiveStep0_b9970262b9a948e9;
        Own<t_btree_000_i__0__1::Type> rel_delta_LiveStep0_749758939b4491e5;
        Own<t_btree_000_i__0__1::Type> rel_DeadlockingThread_8fa18e0d3ee84b8b;
        souffle::RelationWrapper<t_btree_000_i__0__1::Type> wrapper_rel_DeadlockingThread_8fa18e0d3ee84b8b;
        Own<t_btree_000_i__0__1::Type> rel_Live_2818460375647c67;
        souffle::RelationWrapper<t_btree_000_i__0__1::Type> wrapper_rel_Live_2818460375647c67;
        Own<t_btree_000_i__0__1::Type> rel_new_Live_ca472dbac4201e48;
        Own<t_btree_000_i__0__1::Type> rel_delta_Live_2c57e9662e50a2a0;
        Stratum_DeadlockingThread_69894afebfc94aee stratum_DeadlockingThread_18d51e522370d17d;
        Stratum_GCRoot_b08a674c48c5fe0e stratum_GCRoot_491c769852c0cbee;
        Stratum_Live_b9069971975f423e stratum_Live_20b0d7b68b7a9f85;
        Stratum_LiveStep0_41822abe1018780c stratum_LiveStep0_27a7e024cece5ac3;
        Stratum_MaybeDeadlockingThread_fd0c8181097ea422 stratum_MaybeDeadlockingThread_83fe8fd8c29c2f3b;
        Stratum_Reference_374a4e7377ff135c stratum_Reference_abb7727ac43549af;
        std::string inputDirectory;
        std::string outputDirectory;
        SignalHandler *signalHandler{SignalHandler::instance()};
        std::atomic<RamDomain> ctr{};
        std::atomic<std::size_t> iter{};
    };
} // namespace  souffle
namespace souffle
{
    using namespace souffle;
    Sf_ext_stg_gc::Sf_ext_stg_gc() : symTable(),
                                     recordTable(),
                                     regexCache(),
                                     rel_GCRoot_f9754bdf5b76c5df(mk<t_btree_000_i__0__1::Type>()),
                                     wrapper_rel_GCRoot_f9754bdf5b76c5df(0, *rel_GCRoot_f9754bdf5b76c5df, *this, "GCRoot", std::array<const char *, 1>{{"s:symbol"}}, std::array<const char *, 1>{{"val"}}, 0),
                                     rel_MaybeDeadlockingThread_bdb2bd10c95b5982(mk<t_btree_000_i__0__1::Type>()),
                                     wrapper_rel_MaybeDeadlockingThread_bdb2bd10c95b5982(1, *rel_MaybeDeadlockingThread_bdb2bd10c95b5982, *this, "MaybeDeadlockingThread", std::array<const char *, 1>{{"s:symbol"}}, std::array<const char *, 1>{{"threadId"}}, 0),
                                     rel_Reference_c57e388e43703de5(mk<t_btree_000_ii__0_1__11__10::Type>()),
                                     wrapper_rel_Reference_c57e388e43703de5(2, *rel_Reference_c57e388e43703de5, *this, "Reference", std::array<const char *, 2>{{"s:symbol", "s:symbol"}}, std::array<const char *, 2>{{"from", "to"}}, 0),
                                     rel_LiveStep0_d54eeb7faabc4a13(mk<t_btree_000_i__0__1::Type>()),
                                     wrapper_rel_LiveStep0_d54eeb7faabc4a13(3, *rel_LiveStep0_d54eeb7faabc4a13, *this, "LiveStep0", std::array<const char *, 1>{{"s:symbol"}}, std::array<const char *, 1>{{"val"}}, 0),
                                     rel_new_LiveStep0_b9970262b9a948e9(mk<t_btree_000_i__0__1::Type>()),
                                     rel_delta_LiveStep0_749758939b4491e5(mk<t_btree_000_i__0__1::Type>()),
                                     rel_DeadlockingThread_8fa18e0d3ee84b8b(mk<t_btree_000_i__0__1::Type>()),
                                     wrapper_rel_DeadlockingThread_8fa18e0d3ee84b8b(4, *rel_DeadlockingThread_8fa18e0d3ee84b8b, *this, "DeadlockingThread", std::array<const char *, 1>{{"s:symbol"}}, std::array<const char *, 1>{{"threadId"}}, 0),
                                     rel_Live_2818460375647c67(mk<t_btree_000_i__0__1::Type>()),
                                     wrapper_rel_Live_2818460375647c67(5, *rel_Live_2818460375647c67, *this, "Live", std::array<const char *, 1>{{"s:symbol"}}, std::array<const char *, 1>{{"val"}}, 0),
                                     rel_new_Live_ca472dbac4201e48(mk<t_btree_000_i__0__1::Type>()),
                                     rel_delta_Live_2c57e9662e50a2a0(mk<t_btree_000_i__0__1::Type>()),
                                     stratum_DeadlockingThread_18d51e522370d17d(symTable, recordTable, regexCache, pruneImdtRels, performIO, signalHandler, iter, ctr, inputDirectory, outputDirectory, *rel_DeadlockingThread_8fa18e0d3ee84b8b, *rel_LiveStep0_d54eeb7faabc4a13, *rel_MaybeDeadlockingThread_bdb2bd10c95b5982),
                                     stratum_GCRoot_491c769852c0cbee(symTable, recordTable, regexCache, pruneImdtRels, performIO, signalHandler, iter, ctr, inputDirectory, outputDirectory, *rel_GCRoot_f9754bdf5b76c5df),
                                     stratum_Live_20b0d7b68b7a9f85(symTable, recordTable, regexCache, pruneImdtRels, performIO, signalHandler, iter, ctr, inputDirectory, outputDirectory, *rel_delta_Live_2c57e9662e50a2a0, *rel_new_Live_ca472dbac4201e48, *rel_DeadlockingThread_8fa18e0d3ee84b8b, *rel_Live_2818460375647c67, *rel_LiveStep0_d54eeb7faabc4a13, *rel_Reference_c57e388e43703de5),
                                     stratum_LiveStep0_27a7e024cece5ac3(symTable, recordTable, regexCache, pruneImdtRels, performIO, signalHandler, iter, ctr, inputDirectory, outputDirectory, *rel_delta_LiveStep0_749758939b4491e5, *rel_new_LiveStep0_b9970262b9a948e9, *rel_GCRoot_f9754bdf5b76c5df, *rel_LiveStep0_d54eeb7faabc4a13, *rel_Reference_c57e388e43703de5),
                                     stratum_MaybeDeadlockingThread_83fe8fd8c29c2f3b(symTable, recordTable, regexCache, pruneImdtRels, performIO, signalHandler, iter, ctr, inputDirectory, outputDirectory, *rel_MaybeDeadlockingThread_bdb2bd10c95b5982),
                                     stratum_Reference_abb7727ac43549af(symTable, recordTable, regexCache, pruneImdtRels, performIO, signalHandler, iter, ctr, inputDirectory, outputDirectory, *rel_Reference_c57e388e43703de5)
    {
        addRelation("GCRoot", wrapper_rel_GCRoot_f9754bdf5b76c5df, true, false);
        addRelation("MaybeDeadlockingThread", wrapper_rel_MaybeDeadlockingThread_bdb2bd10c95b5982, true, false);
        addRelation("Reference", wrapper_rel_Reference_c57e388e43703de5, true, false);
        addRelation("LiveStep0", wrapper_rel_LiveStep0_d54eeb7faabc4a13, false, false);
        addRelation("DeadlockingThread", wrapper_rel_DeadlockingThread_8fa18e0d3ee84b8b, false, true);
        addRelation("Live", wrapper_rel_Live_2818460375647c67, false, true);
    }

    Sf_ext_stg_gc::~Sf_ext_stg_gc()
    {
    }

    void Sf_ext_stg_gc::runFunction(std::string inputDirectoryArg, std::string outputDirectoryArg, bool performIOArg, bool pruneImdtRelsArg)
    {

        this->inputDirectory = std::move(inputDirectoryArg);
        this->outputDirectory = std::move(outputDirectoryArg);
        this->performIO = performIOArg;
        this->pruneImdtRels = pruneImdtRelsArg;

        // set default threads (in embedded mode)
        // if this is not set, and omp is used, the default omp setting of number of cores is used.
#if defined(_OPENMP)
        if (0 < getNumThreads())
        {
            omp_set_num_threads(static_cast<int>(getNumThreads()));
        }
#endif

        signalHandler->set();
        // -- query evaluation --
        {
            std::vector<RamDomain> args, ret;
            stratum_GCRoot_491c769852c0cbee.run(args, ret);
        }
        {
            std::vector<RamDomain> args, ret;
            stratum_MaybeDeadlockingThread_83fe8fd8c29c2f3b.run(args, ret);
        }
        {
            std::vector<RamDomain> args, ret;
            stratum_Reference_abb7727ac43549af.run(args, ret);
        }
        {
            std::vector<RamDomain> args, ret;
            stratum_LiveStep0_27a7e024cece5ac3.run(args, ret);
        }
        {
            std::vector<RamDomain> args, ret;
            stratum_DeadlockingThread_18d51e522370d17d.run(args, ret);
        }
        {
            std::vector<RamDomain> args, ret;
            stratum_Live_20b0d7b68b7a9f85.run(args, ret);
        }

        // -- relation hint statistics --
        signalHandler->reset();
    }

    void Sf_ext_stg_gc::run()
    {
        runFunction("", "", false, false);
    }

    void Sf_ext_stg_gc::runAll(std::string inputDirectoryArg, std::string outputDirectoryArg, bool performIOArg, bool pruneImdtRelsArg)
    {
        runFunction(inputDirectoryArg, outputDirectoryArg, performIOArg, pruneImdtRelsArg);
    }

    void Sf_ext_stg_gc::printAll([[maybe_unused]] std::string outputDirectoryArg)
    {
        try
        {
            std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(threadId)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(name)_", R"_(DeadlockingThread)_"}, {R"_(operation)_", R"_(output)_"}, {R"_(output-dir)_", R"_(.)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 1, "params": ["threadId"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 1, "types": ["s:symbol"]}})_"}});
            if (!outputDirectoryArg.empty())
            {
                directiveMap["output-dir"] = outputDirectoryArg;
            }
            IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_DeadlockingThread_8fa18e0d3ee84b8b);
        }
        catch (std::exception &e)
        {
            std::cerr << e.what();
            exit(1);
        }
        try
        {
            std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(val)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(name)_", R"_(Live)_"}, {R"_(operation)_", R"_(output)_"}, {R"_(output-dir)_", R"_(.)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 1, "params": ["val"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 1, "types": ["s:symbol"]}})_"}});
            if (!outputDirectoryArg.empty())
            {
                directiveMap["output-dir"] = outputDirectoryArg;
            }
            IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_Live_2818460375647c67);
        }
        catch (std::exception &e)
        {
            std::cerr << e.what();
            exit(1);
        }
    }

    void Sf_ext_stg_gc::loadAll([[maybe_unused]] std::string inputDirectoryArg)
    {
        try
        {
            std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(val)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(fact-dir)_", R"_(.)_"}, {R"_(name)_", R"_(GCRoot)_"}, {R"_(operation)_", R"_(input)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 1, "params": ["val"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 1, "types": ["s:symbol"]}})_"}});
            if (!inputDirectoryArg.empty())
            {
                directiveMap["fact-dir"] = inputDirectoryArg;
            }
            IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_GCRoot_f9754bdf5b76c5df);
        }
        catch (std::exception &e)
        {
            std::cerr << "Error loading GCRoot data: " << e.what() << '\n';
            exit(1);
        }
        try
        {
            std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(threadId)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(fact-dir)_", R"_(.)_"}, {R"_(name)_", R"_(MaybeDeadlockingThread)_"}, {R"_(operation)_", R"_(input)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 1, "params": ["threadId"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 1, "types": ["s:symbol"]}})_"}});
            if (!inputDirectoryArg.empty())
            {
                directiveMap["fact-dir"] = inputDirectoryArg;
            }
            IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_MaybeDeadlockingThread_bdb2bd10c95b5982);
        }
        catch (std::exception &e)
        {
            std::cerr << "Error loading MaybeDeadlockingThread data: " << e.what() << '\n';
            exit(1);
        }
        try
        {
            std::map<std::string, std::string> directiveMap({{R"_(IO)_", R"_(file)_"}, {R"_(attributeNames)_", R"_(from	to)_"}, {R"_(auxArity)_", R"_(0)_"}, {R"_(fact-dir)_", R"_(.)_"}, {R"_(name)_", R"_(Reference)_"}, {R"_(operation)_", R"_(input)_"}, {R"_(params)_", R"_({"records": {}, "relation": {"arity": 2, "params": ["from", "to"]}})_"}, {R"_(types)_", R"_({"ADTs": {}, "records": {}, "relation": {"arity": 2, "types": ["s:symbol", "s:symbol"]}})_"}});
            if (!inputDirectoryArg.empty())
            {
                directiveMap["fact-dir"] = inputDirectoryArg;
            }
            IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_Reference_c57e388e43703de5);
        }
        catch (std::exception &e)
        {
            std::cerr << "Error loading Reference data: " << e.what() << '\n';
            exit(1);
        }
    }

    void Sf_ext_stg_gc::dumpInputs()
    {
        try
        {
            std::map<std::string, std::string> rwOperation;
            rwOperation["IO"] = "stdout";
            rwOperation["name"] = "GCRoot";
            rwOperation["types"] = R"_({"relation": {"arity": 1, "auxArity": 0, "types": ["s:symbol"]}})_";
            IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_GCRoot_f9754bdf5b76c5df);
        }
        catch (std::exception &e)
        {
            std::cerr << e.what();
            exit(1);
        }
        try
        {
            std::map<std::string, std::string> rwOperation;
            rwOperation["IO"] = "stdout";
            rwOperation["name"] = "MaybeDeadlockingThread";
            rwOperation["types"] = R"_({"relation": {"arity": 1, "auxArity": 0, "types": ["s:symbol"]}})_";
            IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_MaybeDeadlockingThread_bdb2bd10c95b5982);
        }
        catch (std::exception &e)
        {
            std::cerr << e.what();
            exit(1);
        }
        try
        {
            std::map<std::string, std::string> rwOperation;
            rwOperation["IO"] = "stdout";
            rwOperation["name"] = "Reference";
            rwOperation["types"] = R"_({"relation": {"arity": 2, "auxArity": 0, "types": ["s:symbol", "s:symbol"]}})_";
            IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_Reference_c57e388e43703de5);
        }
        catch (std::exception &e)
        {
            std::cerr << e.what();
            exit(1);
        }
    }

    void Sf_ext_stg_gc::dumpOutputs()
    {
        try
        {
            std::map<std::string, std::string> rwOperation;
            rwOperation["IO"] = "stdout";
            rwOperation["name"] = "DeadlockingThread";
            rwOperation["types"] = R"_({"relation": {"arity": 1, "auxArity": 0, "types": ["s:symbol"]}})_";
            IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_DeadlockingThread_8fa18e0d3ee84b8b);
        }
        catch (std::exception &e)
        {
            std::cerr << e.what();
            exit(1);
        }
        try
        {
            std::map<std::string, std::string> rwOperation;
            rwOperation["IO"] = "stdout";
            rwOperation["name"] = "Live";
            rwOperation["types"] = R"_({"relation": {"arity": 1, "auxArity": 0, "types": ["s:symbol"]}})_";
            IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_Live_2818460375647c67);
        }
        catch (std::exception &e)
        {
            std::cerr << e.what();
            exit(1);
        }
    }

    SymbolTable &Sf_ext_stg_gc::getSymbolTable()
    {
        return symTable;
    }

    RecordTable &Sf_ext_stg_gc::getRecordTable()
    {
        return recordTable;
    }

    void Sf_ext_stg_gc::setNumThreads(std::size_t numThreadsValue)
    {
        SouffleProgram::setNumThreads(numThreadsValue);
        symTable.setNumLanes(getNumThreads());
        recordTable.setNumLanes(getNumThreads());
        regexCache.setNumLanes(getNumThreads());
    }

    void Sf_ext_stg_gc::executeSubroutine(std::string name, const std::vector<RamDomain> &args, std::vector<RamDomain> &ret)
    {
        if (name == "DeadlockingThread")
        {
            stratum_DeadlockingThread_18d51e522370d17d.run(args, ret);
            return;
        }
        if (name == "GCRoot")
        {
            stratum_GCRoot_491c769852c0cbee.run(args, ret);
            return;
        }
        if (name == "Live")
        {
            stratum_Live_20b0d7b68b7a9f85.run(args, ret);
            return;
        }
        if (name == "LiveStep0")
        {
            stratum_LiveStep0_27a7e024cece5ac3.run(args, ret);
            return;
        }
        if (name == "MaybeDeadlockingThread")
        {
            stratum_MaybeDeadlockingThread_83fe8fd8c29c2f3b.run(args, ret);
            return;
        }
        if (name == "Reference")
        {
            stratum_Reference_abb7727ac43549af.run(args, ret);
            return;
        }
        fatal(("unknown subroutine " + name).c_str());
    }

} // namespace  souffle
namespace souffle
{
    SouffleProgram *newInstance_ext_stg_gc() { return new souffle::Sf_ext_stg_gc; }
    SymbolTable *getST_ext_stg_gc(SouffleProgram *p) { return &reinterpret_cast<souffle::Sf_ext_stg_gc *>(p)->getSymbolTable(); }
} // namespace souffle

#ifndef __EMBEDDED_SOUFFLE__
#include "souffle/CompiledOptions.h"
int main(int argc, char **argv)
{
    try
    {
        souffle::CmdOptions opt(R"_(datalog/ext-stg-gc.dl)_",
                                R"_()_",
                                R"_()_",
                                false,
                                R"_()_",
                                1);
        if (!opt.parse(argc, argv))
            return 1;
        souffle::Sf_ext_stg_gc obj;
#if defined(_OPENMP)
        obj.setNumThreads(opt.getNumJobs());

#endif
        obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
        return 0;
    }
    catch (std::exception &e)
    {
        souffle::SignalHandler::instance()->error(e.what());
    }
}
#endif

namespace souffle
{
    using namespace souffle;
    class factory_Sf_ext_stg_gc : souffle::ProgramFactory
    {
    public:
        souffle::SouffleProgram *newInstance();
        factory_Sf_ext_stg_gc();

    private:
    };
} // namespace  souffle
namespace souffle
{
    using namespace souffle;
    souffle::SouffleProgram *factory_Sf_ext_stg_gc::newInstance()
    {
        return new souffle::Sf_ext_stg_gc();
    }

    factory_Sf_ext_stg_gc::factory_Sf_ext_stg_gc() : souffle::ProgramFactory("ext_stg_gc")
    {
    }

} // namespace  souffle
namespace souffle
{

#ifdef __EMBEDDED_SOUFFLE__
    extern "C"
    {
        souffle::factory_Sf_ext_stg_gc __factory_Sf_ext_stg_gc_instance;
    }
#endif
} // namespace souffle
