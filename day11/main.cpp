#include <algorithm>
#include <array>
#include <iostream>
#include <map>
#include <queue>
#include <ranges>
#include <set>
#include <vector>

#define log std::cout
#define endl std::endl

template <class K> using Vector = std::vector<K>;
template <class K, int size> using Array = std::array<K, size>;
template <class V> using Set = std::set<V>;
template <class K, class V> using Map = std::map<K, V>;
template <class V> using PQ = std::priority_queue<V>;

enum Type { MICROCHIP, GENERATOR };

struct Thing {
  Type type;
  char atom;

  bool operator==(const Thing &other) const {
    return type == other.type && atom == other.atom;
  }

  bool operator<(const Thing &other) const {
    if (type == other.type)
      return atom < other.atom;
    return type < other.type;
  }
};

struct State {
  Array<Set<Thing>, 4> floors;
  int curFloor{0};

  bool operator==(const State &other) const {
    return curFloor == other.curFloor && floors == other.floors;
  }

  bool operator<(const State &other) const {
    if (curFloor != other.curFloor)
      return curFloor < other.curFloor;

    for (int f{0}; f < 4; ++f) {
      auto &floor = floors[f];
      auto &otherFloor = other.floors[f];

      if (floor.size() != otherFloor.size()) {
        return floor.size() < otherFloor.size();
      } else {
        auto oit = otherFloor.begin();
        for (auto &t : floor) {
          auto &otherT = *oit;
          if (!(t == otherT))
            return t < otherT;
          ++oit;
        }
      }
    }
    return false;
  }

  auto &currentFloorItems() { return floors[curFloor]; }
  const auto &currentFloorItems() const { return floors[curFloor]; }

  bool isDone() const {
    return curFloor == 3 && std::all_of(floors.begin(), floors.end() - 1,
                                        [](auto &f) { return f.empty(); });
  }
};

template <typename T> void fillAllSubsets(Vector<Set<T>> &result, Set<T> set) {
  static Set<T> empty;
  result.push_back(empty);

  for (auto &s : set) {
    auto temp = result;

    for (auto &t : temp)
      t.insert(s);

    for (const auto &t : temp)
      result.push_back(t);
  }
}

struct Solver {
  State state;

  using StateStepMap = Map<State, int>;

  static bool isValid(Set<Thing> &ts) {
    // valid if no RTG or every chip has its corresponding RTG.
    auto rtgs = std::any_of(ts.begin(), ts.end(),
                            [&](auto &a) { return a.type == GENERATOR; });

    auto chips = ts | std::ranges::views::filter(
                          [](auto a) { return a.type == MICROCHIP; });

    return !rtgs || std::all_of(chips.begin(), chips.end(), [&](auto &chip) {
      return std::any_of(ts.begin(), ts.end(), [&](auto &a) {
        return a.atom == chip.atom && a.type == GENERATOR;
      });
    });
  }

  static void fillNeigh(Vector<State> &neigh, const State &state) {
    static Vector<Set<Thing>> combos;
    combos.resize(0);
    fillAllSubsets(combos, state.currentFloorItems());

    neigh.resize(0);

    for (auto &combo : combos) {
      if (combo.size() > 0 && combo.size() <= 2) {
        State tempState{state};
        auto &currentFloor = tempState.currentFloorItems();

        for (auto &t : combo)
          currentFloor.erase(t);

        if (isValid(currentFloor)) {
          for (int i{1}; i >= -1; i -= 2) {
            int f = state.curFloor;
            f += i;

            if (f >= 0 && f < 4) {
              State nextState{tempState};
              nextState.curFloor = f;
              auto &newFloor = nextState.currentFloorItems();

              for (auto &t : combo)
                newFloor.insert(t);

              if (isValid(newFloor))
                neigh.push_back(nextState);
            }
          }
        }
      }
    }
  }

  struct Node {
    State state;
    int steps;
    bool operator<(const Node &other) const { return steps > other.steps; }
  };

  int solve() {
    int best{128};
    StateStepMap vis;
    PQ<Node> q;
    Vector<State> neigh;

    q.push({state, 0});

    while (!q.empty()) {
      auto node = q.top();
      q.pop();
      if (node.steps < best) {
        int &s = vis[node.state];
        if (s != 0 && s <= node.steps) {
          continue;
        } else {
          s = node.steps;
        }

        if (node.state.isDone()) {
          best = node.steps;
          log << "found : " << best << endl;
        }

        fillNeigh(neigh, node.state);

        for (auto &n : neigh)
          q.push({n, node.steps + 1});
      }
    }
    return best;
  }
};

int main(void) {
  {
    log << "Solving sample" << endl;
    Solver sol;

    sol.state.floors[0] = {{MICROCHIP, 'H'}, {MICROCHIP, 'L'}};
    sol.state.floors[1] = {{GENERATOR, 'H'}};
    sol.state.floors[2] = {{GENERATOR, 'L'}};

    auto best = sol.solve();
    log << "Sample result : " << best << endl;
  }

  {
    log << "Solving actual input" << endl;
    Solver p1;

    p1.state.floors[0] = {{GENERATOR, 'P'}, {MICROCHIP, 'P'}};
    p1.state.floors[1] = {
        {GENERATOR, 'C'}, {GENERATOR, 'c'}, {GENERATOR, 'R'}, {GENERATOR, 'p'}};
    p1.state.floors[2] = {
        {MICROCHIP, 'C'}, {MICROCHIP, 'c'}, {MICROCHIP, 'R'}, {MICROCHIP, 'p'}};

    auto p1Best = p1.solve();
    log << "Part 1 : " << p1Best << endl;

    Solver p2{p1};
    p2.state.floors[0].insert({GENERATOR, 'E'});
    p2.state.floors[0].insert({MICROCHIP, 'E'});
    p2.state.floors[0].insert({GENERATOR, 'D'});
    p2.state.floors[0].insert({MICROCHIP, 'D'});

    auto p2Best = p2.solve();
    log << "Part 2 : " << p2Best << endl;
  }

  return 0;
}
