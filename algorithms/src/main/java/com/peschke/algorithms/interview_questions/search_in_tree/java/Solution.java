package com.peschke.algorithms.interview_questions.search_in_tree.java;

import java.io.*;
import java.util.*;

public class Solution {
    /**
     * Simple immutable Tree data structure to hold integers
     */
    interface Tree {
        Tree left();
        Tree right();
        Integer value();
        boolean isEmpty();
    }

    /**
     * Node with data and children
     */
    static class ConsNode implements Tree {
        private final Tree    LeftSubTree;
        private final Tree    RightSubTree;
        private final Integer NodeValue;

        ConsNode(final Integer value, final Tree leftSubTree, final Tree rightSubTree) {
            NodeValue = value;
            LeftSubTree = leftSubTree;
            RightSubTree = rightSubTree;
        }

        @Override public Tree    left()    { return LeftSubTree;  }
        @Override public Tree    right()   { return RightSubTree; }
        @Override public Integer value()   { return NodeValue;    }
        @Override public boolean isEmpty() { return false;        }

        @Override public String toString() {
            if (LeftSubTree.isEmpty() && RightSubTree.isEmpty()) return NodeValue.toString();
            return "(" + NodeValue + " " + LeftSubTree + " " + RightSubTree + ")";
        }
        @Override public boolean equals(Object raw) {
            if (raw instanceof ConsNode) {
                ConsNode other = (ConsNode)raw;
                return
                    this.value().equals(other.value()) &&
                    this.left().equals(other.left()) &&
                    this.right().equals(other.right());
            }
            return false;
        }
        @Override public int hashCode() {
            return NodeValue.hashCode() * LeftSubTree.hashCode() * RightSubTree.hashCode();
        }
    }

    /**
     * Terminal Node, contains nothing
     */
    static Tree NilNode = new Tree() {
            @Override public Tree left() {
                throw new RuntimeException("Accessing left node of NilNode");
            }
            @Override public Tree right() {
                throw new RuntimeException("Accessing right node of NilNode");
            }
            @Override public Integer value() {
                throw new RuntimeException("Accessing value of NilNode");
            }

            @Override public boolean isEmpty()            { return true;          }
            @Override public String  toString()           { return "_";           }
            @Override public boolean equals(Object other) { return this == other; }

            @Override public int hashCode() {
                return System.identityHashCode(this);
            }
        };

    /**
     * Factory methods
     */
    static Tree tree() { return NilNode; }
    static Tree tree(Integer value) { return tree(value, tree(), tree()); }
    static Tree tree(Integer value, Tree leftChild, Tree rightChild) {
        return new ConsNode(value, leftChild, rightChild);
    }

    /**
     * Search the tree for the first instance of `pattern`, giving
     * preference to the left child.
     *
     * `pattern` is a tree, representing a pattern of
     * nodes. `NilNode`s should be considered wildcards.
     *
     * Example patterns
     * ----------------
     * `Tree()`                    Matches any tree.
     *
     * `Tree(3)`                   Matches any tree where the root
     *                             node has a value of `3`.
     *
     * `Tree(3, Tree(2), Tree())`  Matches any tree where the root
     *                             node has a value of `3`, and a
     *                             left sub-tree with a root value
     *                             of `2`, and any (or no) right
     *                             sub-tree.
     */
    static Optional<Tree> find(Tree target, Tree pattern) {
        if (pattern.isEmpty()) return Optional.of(target);
        if (target.isEmpty()) return Optional.empty();
        if (prefixMatches(target, pattern)) return Optional.of(target);

        Optional<Tree> leftMatch = find(target.left(), pattern);
        if (leftMatch.isPresent()) return leftMatch;
        return find(target.right(), pattern);
    }

    static boolean prefixMatches(Tree target, Tree pattern) {
        if (pattern.isEmpty()) return true;
        if (target.isEmpty() || target.value() != pattern.value()) return false;

        return
            prefixMatches(target.left(), pattern.left()) &&
            prefixMatches(target.right(), pattern.right());
    }


    static void checkPattern(Tree target, Tree pattern, Optional<Tree> expected) {
        Optional<Tree> result = find(target, pattern);
        String mark = result.equals(expected) ? "[✓]" : "[ ]";
        System.out.println(mark + " " + pattern.toString());
        assert result.equals(expected) : "Expected" + expected + ", but got " + result;
    }

    /**
     * Unit tests
     */
    public static void main(String[] args) {
        final Tree targetTree = tree(1,
                                     tree(2,
                                          tree(3,
                                               tree(4),
                                               tree(3, tree(4), tree(5))),
                                          tree(5,
                                               tree(6,
                                                    tree(7),
                                                    tree(8, tree(9), tree())),
                                               tree(10, tree(11), tree()))),
                                     tree(12));

        System.out.println("    Test Pattern");
        checkPattern(targetTree, tree(), Optional.of(targetTree));

        checkPattern(targetTree, tree(9),  Optional.of(tree(9)));
        checkPattern(targetTree, tree(12), Optional.of(tree(12)));
        checkPattern(targetTree, tree(13), Optional.empty());
        checkPattern(targetTree, tree(1),  Optional.of(targetTree));

        checkPattern(targetTree, tree(2, tree(3), tree(5)),
                     Optional.of(targetTree.left()));
        checkPattern(targetTree, tree(3, tree(4), tree(5)),
                     Optional.of(targetTree.left().left().right()));
        checkPattern(targetTree, tree(6, tree(), tree(8)),
                     Optional.of(targetTree.left().right().left()));
        checkPattern(targetTree, tree(6, tree(), tree(7)),
                     Optional.empty());

        checkPattern(targetTree,
                     tree(5,
                          tree(6,
                               tree(7),
                               tree(8, tree(9), tree())),
                          tree(10, tree(11), tree())),
                     Optional.of(targetTree.left().right()));
    }
}
