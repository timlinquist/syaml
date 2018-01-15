#!groovy

pipeline {
  agent {
    dockerfile true
  }
  environment {
    NEXUS = credentials('exchange-nexus')
  }
  stages {
    stage('Test') {
      steps {
        wrap([$class: 'AnsiColorBuildWrapper', 'colorMapName': 'XTerm']) {
          sh 'sbt clean coverage test coverageReport'
        }
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