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
          withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'sonarqube-official', passwordVariable: 'SONAR_SERVER_TOKEN', usernameVariable: 'SONAR_SERVER_URL']]) {
            sh 'sbt clean coverage test coverageReport sonar'
          }
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