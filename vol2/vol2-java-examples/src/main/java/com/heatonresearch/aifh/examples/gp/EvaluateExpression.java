package com.heatonresearch.aifh.examples.gp;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.genetic.trees.EvaluateTree;
import com.heatonresearch.aifh.genetic.trees.RandomNodeResult;
import com.heatonresearch.aifh.genetic.trees.TreeGenomeNode;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import java.util.Locale;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 4/25/14
 * Time: 6:43 AM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluateExpression extends EvaluateTree {

    public static final int OPCODE_ADD = 0;
    public static final int OPCODE_SUB = 1;
    public static final int OPCODE_DIV = 2;
    public static final int OPCODE_MUL = 3;
    public static final int OPCODE_NEG = 4;
    public static final int OPCODE_POWER = 5;
    public static final int OPCODE_SQRT = 6;
    public static final int OPCODE_VAR_CONST = 7;

    private double[] constValues;
    private int varCount;

    public EvaluateExpression(GenerateRandom rnd, int numConst, int numVar, double minConstValue, double maxConstValue) {
        this.constValues = new double[numConst];
        this.varCount = numVar;

        for(int i=0;i<this.constValues.length;i++) {
            this.constValues[i] = rnd.nextDouble(minConstValue,maxConstValue);
        }
    }

    public EvaluateExpression(GenerateRandom rnd) {
        this(rnd,100,1,-5,5);
    }

    public int determineChildCount(int opcode) {
        switch(opcode) {
            case OPCODE_ADD: return 2;
            case OPCODE_SUB: return 2;
            case OPCODE_DIV: return 2;
            case OPCODE_MUL: return 2;
            case OPCODE_NEG: return 1;
            case OPCODE_POWER: return 2;
            case OPCODE_SQRT: return 1;
            default:
                return 0;
        }
    }

    public String getOpcodeText(int opcode) {
        switch(opcode) {
            case OPCODE_NEG:
                return "-";
            case OPCODE_ADD:
                return "+";
            case OPCODE_SUB:
                return "-";
            case OPCODE_DIV:
                return "/";
            case OPCODE_MUL:
                return "*";
            case OPCODE_POWER:
                return "^";
            case OPCODE_SQRT:
                return "sqrt";
            default:
                int index = opcode - OPCODE_VAR_CONST;
                if( index>=(this.constValues.length+varCount) ) {
                    throw new AIFHError("Invalid opcode: " + opcode);
                }
                if( index<this.varCount) {
                    return ""+((char)('a'+index));
                } else {
                    return String.format(Locale.US, "%.8f", this.constValues[index-varCount]);
                }
        }
    }

    public String displayExpressionLISP(TreeGenomeNode node) {
        StringBuilder result = new StringBuilder();

        if( determineChildCount(node.getOpcode())==0 ) {
            result.append(getOpcodeText(node.getOpcode()));
        } else {
            result.append("(");
            result.append(getOpcodeText(node.getOpcode()));
            for(TreeGenomeNode child: node.getChildren()) {
                result.append(" ");
                result.append(displayExpressionLISP(child));
            }
            result.append(")");
        }

        return result.toString();
    }

    public String displayExpressionNormal(TreeGenomeNode node) {
        StringBuilder result = new StringBuilder();

        if( determineChildCount(node.getOpcode())==0 ) {
            result.append(getOpcodeText(node.getOpcode()));
        } else {
            int childCount = determineChildCount(node.getOpcode());

            if( childCount==0  ) {
                result.append(getOpcodeText(node.getOpcode()));
            } else {
                String name = getOpcodeText(node.getOpcode());

                if( name.length()>1 ) {
                    result.append(name);
                    result.append("(");
                    boolean first = true;
                    for(TreeGenomeNode child: node.getChildren()) {
                        if( !first ) {
                            result.append(",");
                        }
                        result.append(displayExpressionNormal(child));
                        first = false;
                    }
                    result.append(")");
                } else {
                    result.append("(");
                    if( childCount==2 ) {
                        result.append(displayExpressionNormal(node.getChildren().get(0)));
                        result.append(name);
                        result.append(displayExpressionNormal(node.getChildren().get(1)));
                        result.append(")");
                    } else {
                        result.append(name);
                        result.append(displayExpressionNormal(node.getChildren().get(0)));
                        result.append(")");
                    }
                }
            }

        }


        return result.toString();
    }

    @Override
    public int getVarConstOpcode() {
        return OPCODE_VAR_CONST;
    }

    @Override
    public int getNumConst() {
        return this.constValues.length;
    }

    @Override
    public int getNumVar() {
        return this.varCount;
    }

    @Override
    public double evaluate(TreeGenomeNode node, double[] varValues) {
        switch(node.getOpcode()) {
            case OPCODE_NEG:
                return -(evaluate(node.getChildren().get(0), varValues));
            case OPCODE_ADD:
                return evaluate(node.getChildren().get(0),varValues) + evaluate(node.getChildren().get(1),varValues);
            case OPCODE_SUB:
                return evaluate(node.getChildren().get(0),varValues) - evaluate(node.getChildren().get(1),varValues);
            case OPCODE_DIV:
                return evaluate(node.getChildren().get(0),varValues) / evaluate(node.getChildren().get(1),varValues);
            case OPCODE_MUL:
                return evaluate(node.getChildren().get(0),varValues) * evaluate(node.getChildren().get(1),varValues);
            case OPCODE_POWER:
                return Math.pow(evaluate(node.getChildren().get(0),varValues), evaluate(node.getChildren().get(1),varValues));
            case OPCODE_SQRT:
                return Math.sqrt(evaluate(node.getChildren().get(0),varValues));
            default:
                int index = node.getOpcode() - OPCODE_VAR_CONST;
                if( index>=(this.constValues.length+varCount) ) {
                    throw new AIFHError("Invalid opcode: " + node.getOpcode());
                }
                if( index<this.varCount) {
                    return varValues[index];
                } else {
                    return this.constValues[index-this.varCount];
                }
        }

    }
}
