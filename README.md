Artificial Neural Networks in Akka
==================================

### Premise

  This project is an attempt to use an artificial neural network as a data flow transformer. Having an input stream of symbols which can be decoded into an input vector, the network will be able to generate a stream of more abstract symbols, using as additional information both the context (ie. data which was received before) and time gaps between consecutive chunks of data. The network is reactive - its computations are triggered by the input stream itself, not by a third agent watching the stream and sending requests to the network - and it can simultaneously receive and generate data.
  
### Documentation
  * [Abstract]
  * [Full article]
  * [A video from my gig on Scalar 2017](https://www.youtube.com/watch?v=5r4LSQT7Uc4&t=1s)
  * [Further ideas]

If you want to look at only one class in this project then it's probably [this one](https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/src/main/scala/anna/async/Neuron.scala).

You can also check my other project: [GAI Library](https://github.com/makingthematrix/gailibrary), a small library for Artificial Intelligence in computer games, based on cellular automata. I focus on it now and I use it to learn Rust. When I develop it to a point when it's possible to use it in practical cases I plan to come back to ANN, and then switch between the two from time to time. (Hopefully I won't start writing yet another one).

### How to Install
  1. You will need Java JDK 7+ and sbt 0.13.8 or newer.
    * [OpenJDK]– for Linux
    * [Java (Oracle)] – for any platform (Linux also)
    * [sbt - Linux]
    * [sbt - Windows] 
        * The MSI installer is no longer supported, as far as I know, so you have to download and unpack the zip file.
    * [sbt - Mac] 
  2. You can check if sbt works simply typing **sbt** in the command line
  3. Download the ANNA project, either through GIT, or by downloading the zip file. The current stable branch is `master`.
  4. Go to the main project directory (“ann”) and type `sbt compile`. **sbt** will download and install Scala and Akka if you haven't done it before. Be patient. Then type `sbt console`. You will see the welcome screen. Type `help`.

   [Abstract]: <https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/doc/ArtificialNeuralNetworksInAkka-abstract.pdf>
   [Full article]: <https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/doc/ArtificialNeuralNetworksInAkka.pdf>
   [Further ideas]: <https://github.com/makingthematrix/ann/blob/SOSWithBlock_1.0/doc/FurtherIdeas.pdf>
   [OpenJDK]: <http://openjdk.java.net/install/>
   [Java (Oracle)]: <http://www.oracle.com/technetwork/java/javase/downloads/index.html>
   [sbt - Linux]: <http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Linux.html>
   [sbt - Windows]: <http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Windows.html>
   [sbt - Mac]: <http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Mac.html>
