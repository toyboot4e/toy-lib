#!/usr/bin/env stack
{- stack script --resolver lts-21.6 --package array --package bytestring --package containers --package deepseq --package extra --package hashable --package unordered-containers --package heaps --package utility-ht --package vector --package vector-algorithms --package primitive --package random --package --package quickcheck transformers --ghc-options "-D DEBUG" -}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
{-# RULES "Force inline VAI.sort" VAI.sort = VAI.sortBy compare #-}
