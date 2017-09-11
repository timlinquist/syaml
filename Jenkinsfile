#!groovy

pipeline {
  agent {
    docker {
      image 'mulesoft/maelstrom-sbtnode-builder:v0.2.3'
      registryUrl 'https://devdocker.mulesoft.com:18078'
      registryCredentialsId 'quay-docker-registry'
    }
  }
  environment {
    NEXUS = credentials('nexus')
  }
  stages {
    stage('Test') {
      steps {
        sh 'sbt clean coverage test coverageReport'
      }
    }
    stage('Publish') {
      steps {
        sh 'sbt syamlJS/publish'
        sh 'sbt syamlJVM/publish'
      }
    }
  }
}