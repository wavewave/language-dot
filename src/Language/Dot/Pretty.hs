{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Dot.Pretty
  ( prettyPrintDot
  , renderDot
  )
  where

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Numeric
import Prettyprinter
import Prettyprinter.Render.String

import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

prettyPrintDot :: Graph -> Doc ()
prettyPrintDot = pretty

renderDot :: Graph -> String
renderDot = renderString . layoutPretty defaultLayoutOptions . pretty

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance Pretty Graph where
  pretty (Graph s d mi ss) =
      vcat [ pretty s <+> pp d <+> pp mi <+> lbrace
           , indent' (vcat' ss)
           , rbrace
           ]

instance Pretty GraphStrictness where
  pretty StrictGraph   = pretty "strict"
  pretty UnstrictGraph = mempty

instance Pretty GraphDirectedness where
  pretty DirectedGraph   = pretty "digraph"
  pretty UndirectedGraph = pretty "graph"

instance Pretty Id where
  pretty (NameId v)    = pretty v
  pretty (StringId v)  = dquotes (pretty v)
  pretty (IntegerId v) = pretty v
  pretty (FloatId v)   = ffloat v
  pretty (XmlId v)     = langle <> pp v <> rangle

instance Pretty Statement where
  pretty (NodeStatement ni as)       = pp ni <+> if not (null as) then brackets (hsep' as) else mempty
  pretty (EdgeStatement es as)       = hsep' es <+> if not (null as) then brackets (hsep' as) else mempty
  pretty (AttributeStatement t as)   = pp t <+> brackets (hsep' as)
  pretty (AssignmentStatement i0 i1) = pp i0 <> equals <> pp i1
  pretty (SubgraphStatement s)       = pp s

instance Pretty AttributeStatementType where
  pretty GraphAttributeStatement = pretty "graph"
  pretty NodeAttributeStatement  = pretty "node"
  pretty EdgeAttributeStatement  = pretty "edge"

instance Pretty Attribute where
  pretty (AttributeSetTrue i)      = pp i
  pretty (AttributeSetValue i0 i1) = pp i0 <> equals <> pp i1

instance Pretty NodeId where
  pretty (NodeId i mp) = pp i <> pp mp

instance Pretty Port where
  pretty (PortI i mc) = colon <> pp i <> maybe mempty ((colon <>) . pp) mc
  pretty (PortC c)    = colon <> pp c

instance Pretty Compass where
  pretty CompassN  = pretty "n"
  pretty CompassE  = pretty "e"
  pretty CompassS  = pretty "s"
  pretty CompassW  = pretty "w"
  pretty CompassNE = pretty "ne"
  pretty CompassNW = pretty "nw"
  pretty CompassSE = pretty "se"
  pretty CompassSW = pretty "sw"

instance Pretty Subgraph where
  pretty (NewSubgraph mi ss) = vcat [ pretty "subgraph" <+> pp mi <+> lbrace
                                    , indent' (vcat' ss)
                                    , rbrace
                                    ]
  pretty (SubgraphRef i)     = pretty "subgraph" <+> pp i

instance Pretty Entity where
  pretty (ENodeId et ni)   = pp et <+> pp ni
  pretty (ESubgraph et sg) = pp et <+> pp sg

instance Pretty EdgeType where
  pretty NoEdge         = mempty
  pretty DirectedEdge   = pretty "->"
  pretty UndirectedEdge = pretty "--"

instance Pretty Xml where
  pretty (XmlEmptyTag n as) = langle <> pp n <+> hsep' as <> slash <> rangle
  pretty (XmlTag n as xs)   = langle <> pp n <+> hsep' as <> rangle <> hcat' xs <> langle <> slash <> pp n <> rangle
  pretty (XmlText t)        = pretty t

instance Pretty XmlName where
  pretty (XmlName n) = pretty n

instance Pretty XmlAttribute where
  pretty (XmlAttribute n v) = pp n <> equals <> pp v

instance Pretty XmlAttributeValue where
  pretty (XmlAttributeValue v) = dquotes (pretty v)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

pp :: (Pretty a) => a -> Doc b
pp = pretty

indent' :: Doc a -> Doc a
indent' = indent 2

hcat' :: (Pretty a) => [a] -> Doc b
hcat' = hcat . map pretty

hsep' :: (Pretty a) => [a] -> Doc b
hsep' = hsep . map pretty

vcat' :: (Pretty a) => [a] -> Doc b
vcat' = vcat . map pretty

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

ffloat :: Float -> Doc a
ffloat v = pretty (showFFloat Nothing v "")
