using System;
using System.Collections;
using System.Text.RegularExpressions;

class Node {
    public string id;
    public List<int> values = new List<int>();
    public string targetLow;
    public string targetHigh;
}

class Solver
{
    public string Input { get; }

    public bool IsSample { get; }

    public Solver(string input, bool isSample)
    {
        this.Input = input;
        this.IsSample = isSample;
    }

    public void Part1()
    {
        var targetMin = this.IsSample ? 2 : 17;
        var targetMax = this.IsSample ? 5 : 61;
        var res = Execute(Parse()).Single(v => v.min == targetMin && v.max == targetMax).id.Split(' ')[1];
        Console.WriteLine($"Part 1 : {res}");
    }

    public void Part2()
    {
        var machine = Execute(Parse()).Last().machine;
        Console.WriteLine($"Part 2 : {machine["output 0"].values.Single() *
                                      machine["output 1"].values.Single() *
                                      machine["output 2"].values.Single()}");
    }

    IEnumerable<(Dictionary<string, Node> machine, string id, int min, int max)>
        Execute(Dictionary<string, Node> machine)
    {
        var done = false;
        while (!done)
        {
            done = true;
            foreach (var node in machine.Values)
            {
                if (node.values.Count == 2)
                {
                    done = false;
                    var min = node.values.Min();
                    var max = node.values.Max();
                    machine[node.targetLow].values.Add(min);
                    machine[node.targetHigh].values.Add(max);
                    node.values.Clear();
                    yield return (machine, node.id, min, max);
                }
            }
        }
    }

    Dictionary<string, Node> Parse()
    {
        var m = new Dictionary<string, Node>();
        void newNodes(params string[] ids)
        {
            foreach (var id in ids)
                if (!m.ContainsKey(id))
                    m[id] = new Node { id = id };
        }
        foreach (var line in this.Input.Split("\n"))
        {
            if (Match(line, @"(.+) gives low to (.+) and high to (.+)", out var match))
            {
                newNodes(match);
                m[match[0]].targetLow = match[1];
                m[match[0]].targetHigh = match[2];
            }
            else if (Match(line, @"value (\d+) goes to (.+)", out match))
            {
                newNodes(match[1]);
                m[match[1]].values.Add(int.Parse(match[0]));
            }
            else
            {
                throw new ArgumentException($"Invalid instruction : {line}");
            }
        }
        return m;
    }

    bool Match(string s, string pat, out string[] match)
    {
        var ma = Regex.Match(s, pat);
        match = null;
        if (ma.Success)
        {
            match = ma.Groups.Cast<Group>().Skip(1).Select(g => g.Value).ToArray();
            return true;
        }
        return false;
    }
}

foreach (var fp in Args) {
    Console.WriteLine($"Solving for file : {fp}");
    Solver solver = new Solver(System.IO.File.ReadAllText(fp).Trim(), fp == "sample.txt");
    solver.Part1();
    solver.Part2();
}

