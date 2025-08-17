//! Traits and functions used to generate visualizations for Directed Cyclic Graphs.
use crate::common::Variable;

pub type NodeHandle = usize;

pub trait Graph<N: Node> {
    /// All the entrypoints of the graph, i.e. Nodes marking the start of a function.
    fn entrypoints(&self) -> Vec<NodeHandle>;

    /// All the nodes of the graph.
    fn nodes(&self) -> &[N];
}

pub trait Node {
    /// List of all the predecssors of the node.
    fn prev(&self) -> Vec<NodeHandle>;
    /// List of all the followers of the node.
    fn next(&self) -> Vec<NodeHandle>;

    /// Formats the content of the node into a readable string, using the given `varfmt` in case it needs to format
    /// [`Variables`][crate::common::Variable].
    fn label<F: Fn(&Variable) -> String>(&self, varfmt: F) -> String;
}

mod visualisation {
    mod flowchart {
        use std::{collections::BTreeSet, fmt::Debug};

        use crate::{
            common::Variable,
            graph::{self, Graph, Node},
        };

        #[derive(Debug)]
        struct FlowNode {
            id: usize,
            children: Vec<FlowNode>,
            backward_edges: Vec<usize>,
            label: String,
        }

        fn compile_nodes<N, G>(graph: &G) -> Vec<FlowNode>
        where
            N: Node,
            G: Graph<N>,
        {
            fn compile_nodes_rec<N, G>(
                graph: &G,
                existing_nodes: &mut BTreeSet<usize>,
                id: graph::NodeHandle,
            ) -> FlowNode
            where
                N: Node,
                G: Graph<N>,
            {
                existing_nodes.insert(id);
                let mut children = vec![];
                let mut backward_edges = vec![];

                for next in graph.nodes()[id].next() {
                    if existing_nodes.contains(&next) {
                        backward_edges.push(next);
                    } else {
                        children.push(compile_nodes_rec(graph, existing_nodes, next));
                    }
                }

                FlowNode {
                    id,
                    children,
                    backward_edges,
                    label: graph.nodes()[id].label(Variable::default_fmt),
                }
            }

            graph
                .entrypoints()
                .into_iter()
                .map(|hdx| compile_nodes_rec(graph, &mut Default::default(), hdx))
                .collect()
        }

        fn generate_flowchart_str(nodes: Vec<FlowNode>) -> String {
            fn generate_flowchart_str_rec(node: FlowNode, indent: usize) -> String {
                let indent_str = " ".repeat(indent);
                let internal_indent_str = " ".repeat(indent + 2);

                let mut str = format!("{indent_str}{} #{}\n", node.label, node.id);

                for be in node.backward_edges {
                    str.push_str(&internal_indent_str);
                    str.push_str(&format!("(#{be})"));
                    str.push('\n');
                }

                for child in node.children {
                    str.push_str(&generate_flowchart_str_rec(child, indent + 2));
                }

                str
            }

            nodes
                .into_iter()
                .map(|n| generate_flowchart_str_rec(n, 0))
                .collect::<Vec<_>>()
                .join("\n")
        }

        pub fn generate_flowchart<N, G>(graph: G) -> String
        where
            N: Node,
            G: Graph<N>,
        {
            let rot = compile_nodes(&graph);
            generate_flowchart_str(rot)
        }
    }

    pub mod mermaid {
        use crate::{
            common::Variable,
            graph::{Graph, Node},
        };

        pub fn generate_mermaid<N, G>(graph: G) -> String
        where
            N: Node,
            G: Graph<N>,
        {
            let mut edges = vec![];
            let mut nodes = vec![];

            let mut to_process = graph.entrypoints();

            while let Some(hdx) = to_process.pop() {
                nodes.push(hdx);

                for next in graph.nodes()[hdx].next() {
                    edges.push((hdx, next));

                    if !nodes.contains(&next) {
                        to_process.push(next);
                    }
                }
            }

            let mut str = "flowchart\n".to_owned();
            for node in &nodes {
                str.push_str(&format!(
                    "  {node}[\"{}\"]\n",
                    graph.nodes()[*node].label(Variable::html_fmt)
                ));
            }

            for (from, to) in edges {
                str.push_str(&format!("  {from} --> {to}\n"));
            }

            for node in nodes {
                if graph.nodes()[node].next().len() > 1 || graph.nodes()[node].prev().len() > 1 {
                    str.push_str(&format!("  {node}@{{ shape: diam}}\n"));
                }
            }

            for node in graph.entrypoints() {
                str.push_str(&format!("  {node}@{{ shape: card}}\n"));
            }

            str
        }
    }

    /// Graph formats supported by the visualizer.
    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum)]
    pub enum GraphType {
        /// <https://www.mermaidchart.com/>
        Mermaid,
        /// <https://flowchart.fun/>
        Flowchart,
    }

    /// Generate a formatted string that can be used with the visualizer of the given `r#type`.
    pub fn generate_representation<N, G>(graph: G, r#type: GraphType) -> String
    where
        N: super::Node,
        G: super::Graph<N>,
    {
        match r#type {
            GraphType::Mermaid => mermaid::generate_mermaid(graph),
            GraphType::Flowchart => flowchart::generate_flowchart(graph),
        }
    }
}

pub use visualisation::{GraphType, generate_representation};
