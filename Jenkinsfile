#!groovy
@Library('amf-jenkins-library') _

def SLACK_CHANNEL = '#amf-jenkins'
def PRODUCT_NAME = "syaml"
def lastStage = ""
def color = '#FF8C00'
def headerFlavour = "WARNING"

pipeline {
    options {
        timeout(time: 30, unit: 'MINUTES')
        ansiColor('xterm')
    }
    agent {
        dockerfile {
            filename 'Dockerfile'
            registryCredentialsId 'github-salt'
            registryUrl 'https://ghcr.io'
        }
    }
    environment {
        NEXUS = credentials('exchange-nexus')
        GITHUB_ORG = 'aml-org'
        GITHUB_REPO = 'syaml'
    }
    stages {
        stage('Test') {
            steps {
                script {
                    lastStage = env.STAGE_NAME
                    sh 'sbt clean coverage test coverageAggregate'
                }
            }
        }
        stage('Coverage') {
            when {
                anyOf {
                    branch 'master'
                }
            }
            steps {
                withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'sonarqube-official', passwordVariable: 'SONAR_SERVER_TOKEN', usernameVariable: 'SONAR_SERVER_URL']]) {
                    script {
                        lastStage = env.STAGE_NAME
                        sh 'sbt -Dsonar.host.url=${SONAR_SERVER_URL} -Dsonar.login=${SONAR_SERVER_TOKEN} sonarScan'
                    }
                }
            }
        }
        stage('Publish') {
            when {
                anyOf {
                    branch 'master'
                }
            }
            steps {
                wrap([$class: 'AnsiColorBuildWrapper', 'colorMapName': 'XTerm']) {
                    script {
                        lastStage = env.STAGE_NAME
                        sh '''
                               echo "about to publish in sbt"
                               sbt syamlJS/publish
                               sbt syamlJVM/publish
                               echo "sbt publishing successful"
                           '''
                    }
                }
            }
        }
        stage('Tag version') {
            when {
                branch 'master'
            }
            steps {
                withCredentials([[$class: 'UsernamePasswordMultiBinding', credentialsId: 'github-salt', passwordVariable: 'GITHUB_PASS', usernameVariable: 'GITHUB_USER']]) {
                    script {
                        lastStage = env.STAGE_NAME
                        def version = sbtArtifactVersion("syamlJVM")
                        tagCommitToGithub(version)
                    }
                }
            }
        }
    }
    post {
        unsuccessful {
            script {
                if (isMaster() || isDevelop()) {
                    sendBuildErrorSlackMessage(lastStage, SLACK_CHANNEL, PRODUCT_NAME)
                } else {
                    echo "Unsuccessful build: skipping slack message notification as branch is not master or develop"
                }
            }
        }
        success {
            script {
                echo "SUCCESSFUL BUILD"
                if (isMaster()) {
                    sendSuccessfulSlackMessage(SLACK_CHANNEL, PRODUCT_NAME)
                } else {
                    echo "Successful build: skipping slack message notification as branch is not master"
                }
            }
        }
    }
}

Boolean isDevelop() {
    env.BRANCH_NAME == "develop"
}

Boolean isMaster() {
    env.BRANCH_NAME == "master"
}

def sendBuildErrorSlackMessage(String lastStage, String slackChannel, String productName) {
    def color = '#FF8C00'
    def headerFlavour = 'WARNING'
    if (isMaster()) {
        color = '#FF0000'
        headerFlavour = "RED ALERT"
    } else if (isDevelop()) {
        color = '#FFD700'
    }
    def message = """:alert: ${headerFlavour}! :alert: Build failed!.
                  |Branch: ${env.BRANCH_NAME}
                  |Stage: ${lastStage}
                  |Product: ${productName}
                  |Build URL: ${env.BUILD_URL}""".stripMargin().stripIndent()
    slackSend color: color, channel: "${slackChannel}", message: message
}

def sendSuccessfulSlackMessage(String slackChannel, String productName) {
    slackSend color: '#00FF00', channel: "${slackChannel}", message: ":ok_hand: ${productName} Master Publish OK! :ok_hand:"
}

