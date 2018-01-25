# Requirements

 * why3
 * z3

# Building


    sbt assembly

# Docker

To build the Repliss docker image, run:
    
    docker build -t repliss .


To run the image:

    docker run --rm -p "8080:8080" -it repliss
    
