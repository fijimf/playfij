package analysis.junkyard

trait ActivationFunction {
  def f(x: Double): Double

  def df(x: Double): Double
}

object Sigmoid extends ActivationFunction {
  def f(x: Double) = 1.0 / (1.0 + math.exp(-1.0 * x))

  def df(x: Double) = x * (1.0 - x)
}


case class Neuron(w: List[Double], a: ActivationFunction) {
  def response(i: List[Double]): Double = {
    a.f(i.zip(w).map(x => x._1 * x._2).sum)
  }

  def partial(i: List[Double]): Double = {
    a.df(i.zip(w).map(x => x._1 * x._2).sum)
  }
}

case class Layer(neurons: List[Neuron]) {
  def response(i: List[Double]): List[Double] = {
    neurons.map(_.response(i))
  }

  def partial(i: List[Double]): List[Double] = {
    neurons.map(_.partial(i))
  }
}

case class Network(layers: List[Layer]) {
  def response(inputs: List[Double]): List[Double] = {
    layers.foldLeft(inputs) {
      case (inputs: List[Double], layer: Layer) => layer.response(inputs)
    }
  }

  def partials(inputs: List[Double]): List[List[Double]] = {
    layers.foldLeft(inputs, List.empty[List[Double]]) {
      case ((in, accum), layer) => {
        (layer.response(in), layer.partial(in) :: accum)
      }
    }._2
  }
}
//
//
//        def errors(inputs: List[Double], outputs: List[Double]): List[List[Double]] = {
//          val e = response(inputs).zip(outputs).map(x => x._1 - x._2)
//          partials(inputs).reverse.foldLeft((e,List.empty[List[Double]])){case ((err,accum), p) =>{
//            val errs: List[Double] = e.zip(p).map(tup => tup._1 * tup._2)
//
//
//          }} ._2
//
//        }
//
//    }
//
//
//
//    double[] input = inputs[i];
//    double[] expectedOutput = expectedOutputs[i];
//
//    List < Layer > layers = neuralNetwork.getLayers();
//
//    neuralNetwork.setInputs(input);
//    double[] output = neuralNetwork.getOutput();
//
//    //First step of the backpropagation algorithm. Backpropagate errors from the output layer all the way up
//    //to the first hidden layer
//    for (int j = layers.size() - 1;
//    j > 0;
//    j --)
//    {
//      Layer layer = layers.get(j);
//
//      for (int k = 0;
//      k < layer.getNeurons().size();
//      k ++)
//      {
//        Neuron neuron = layer.getNeurons().get(k);
//        double neuronError = 0;
//
//        if (layer.isOutputLayer()) {
//          //the order of output and expected determines the sign of the delta. if we have output - expected, we subtract the delta
//          //if we have expected - output we add the delta.
//          neuronError = neuron.getDerivative() * (output[k] - expectedOutput[k]);
//        } else {
//          neuronError = neuron.getDerivative();
//
//          double sum = 0;
//          List < Neuron > downstreamNeurons = layer.getNextLayer().getNeurons();
//          for (Neuron downstreamNeuron: downstreamNeurons)
//          {
//
//            int l = 0;
//            boolean found = false;
//            while (l < downstreamNeuron.getInputs().size() && !found) {
//              Synapse synapse = downstreamNeuron.getInputs().get(l);
//
//              if (synapse.getSourceNeuron() == neuron) {
//                sum += (synapse.getWeight() * downstreamNeuron.getError());
//                found = true;
//              }
//
//              l ++;
//            }
//          }
//
//          neuronError *= sum;
//        }
//
//        neuron.setError(neuronError);
//      }
//    }
//
//    //Second step of the backpropagation algorithm. Using the errors calculated above, update the weights of the
//    //network
//    for (int j = layers.size() - 1;
//    j > 0;
//    j --)
//    {
//      Layer layer = layers.get(j);
//
//      for (Neuron neuron: layer.getNeurons())
//      {
//
//        for (Synapse synapse: neuron.getInputs())
//        {
//
//          double newLearningRate = characteristicTime > 0 ? learningRate / (1 + (currentEpoch / characteristicTime)): learningRate;
//          double delta = newLearningRate * neuron.getError() * synapse.getSourceNeuron().getOutput();
//
//          if (synapseNeuronDeltaMap.get(synapse) != null) {
//            double previousDelta = synapseNeuronDeltaMap.get(synapse);
//            delta += momentum * previousDelta;
//          }
//
//          synapseNeuronDeltaMap.put(synapse, delta);
//          synapse.setWeight(synapse.getWeight() - delta);
//        }
//      }
//    }
//
//    output = neuralNetwork.getOutput();
//    error += error(output, expectedOutput);
//  }
//
//  return error;
//}
//
//private String explode (double[] array) {
//String string = "[";
//
//for (double number: array) {
//string += number + ", ";
//}
//
//Pattern pattern = Pattern.compile (", $", Pattern.DOTALL);
//Matcher matcher = pattern.matcher (string);
//string = matcher.replaceAll ("");
//
//return string + "]";
//}
//
//public double error (double[] actual, double[] expected) {
//
//if (actual.length != expected.length) {
//throw new IllegalArgumentException ("The lengths of the actual and expected value arrays must be equal");
//}
//
//double sum = 0;
//
//for (int i = 0;
//i < expected.length;
//i ++) {
//sum += Math.pow (expected[i] - actual[i], 2);
//}
//
//return sum / 2;
//}
//}
//
