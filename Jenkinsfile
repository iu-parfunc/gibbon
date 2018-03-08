pipeline {
  // This is designed to run on Cutter @ IU
  agent {
    label 'linux-ubuntu-1404'
  }

  triggers {
      // Try to create a webhook:
      pollSCM('')
  }

  stages {
    stage('Build') {
      steps {
        sh "./.jenkins_script.sh"
      }
    }
  }

  post {
    failure {
      slackSend (channel: "#treetraversals", color: '#FF0000', message: "FAILED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]' (${env.BUILD_URL})")
    }
  }
}
