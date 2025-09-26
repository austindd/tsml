#!/bin/bash
echo "const x = 0; return x;" | roc dev main.roc 2>&1 | grep -A 2 "Type Inference"