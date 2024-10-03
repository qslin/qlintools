# Use rocker/rstudio as the base image
FROM rocker/rstudio:latest

# Set the working directory to /home/rstudio
WORKDIR /home/rstudio

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Expose the default RStudio port
EXPOSE 8787

# Set the default command
CMD ["/init"]

