library(shiny)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Breakout Game"),
  tags$head(
    tags$style(
      HTML("
        #gameCanvas {
          border: 1px solid black;
          background-color: #eee;
          display: block;
          margin-left: auto;
          margin-right: auto;
        }
      ")
    ),
    tags$script(
      HTML("
        let canvas, ctx, ball, paddle, bricks, gameState, startTime, intervalID;
        
        function initializeGame(speed) {
          canvas = document.getElementById('gameCanvas');
          ctx = canvas.getContext('2d');
          ball = { x: canvas.width / 2, y: canvas.height - 30, dx: speed, dy: -speed, radius: 10 };
          paddle = { height: 10, width: 75, x: (canvas.width - 75) / 2 };
          bricks = [];
          for(let c = 0; c < 5; c++) {
            bricks[c] = [];
            for(let r = 0; r < 3; r++) {
              bricks[c][r] = { x: 0, y: 0, status: 1 };
            }
          }
          gameState = { ballSpeed: speed, rightPressed: false, leftPressed: false, score: 0, level: 1 };
          document.addEventListener('keydown', keyDownHandler, false);
          document.addEventListener('keyup', keyUpHandler, false);
          startTime = new Date().getTime();
          intervalID = setInterval(draw, 10);
        }
        
        function draw() {
          ctx.clearRect(0, 0, canvas.width, canvas.height);
          drawBricks();
          drawBall();
          drawPaddle();
          collisionDetection();
          if(ball.x + ball.dx > canvas.width - ball.radius || ball.x + ball.dx < ball.radius) {
            ball.dx = -ball.dx;
          }
          if(ball.y + ball.dy < ball.radius) {
            ball.dy = -ball.dy;
          } else if(ball.y + ball.dy > canvas.height - ball.radius) {
            if(ball.x > paddle.x && ball.x < paddle.x + paddle.width) {
              ball.dy = -ball.dy;
            } else {
              document.location.reload();
            }
          }
          if(gameState.rightPressed && paddle.x < canvas.width - paddle.width) {
            paddle.x += 7;
          } else if(gameState.leftPressed && paddle.x > 0) {
            paddle.x -= 7;
          }
          ball.x += ball.dx;
          ball.y += ball.dy;
        }
        
        function keyDownHandler(e) {
          if(e.key == 'Right' || e.key == 'ArrowRight') {
            gameState.rightPressed = true;
          } else if(e.key == 'Left' || e.key == 'ArrowLeft') {
            gameState.leftPressed = true;
          }
        }
        
        function keyUpHandler(e) {
          if(e.key == 'Right' || e.key == 'ArrowRight') {
            gameState.rightPressed = false;
          } else if(e.key == 'Left' || e.key == 'ArrowLeft') {
            gameState.leftPressed = false;
          }
        }
        
        function drawBricks() {
          for(let c = 0; c < bricks.length; c++) {
            for(let r = 0; r < bricks[c].length; r++) {
              if(bricks[c][r].status == 1) {
                let brickX = (c * (75 + 10)) + 30;
                let brickY = (r * (20 + 10)) + 30;
                bricks[c][r].x = brickX;
                bricks[c][r].y = brickY;
                ctx.beginPath();
                ctx.rect(brickX, brickY, 75, 20);
                ctx.fillStyle = '#0095DD';
                ctx.fill();
                ctx.closePath();
              }
            }
          }
        }
        
        function drawBall() {
          ctx.beginPath();
          ctx.arc(ball.x, ball.y, ball.radius, 0, Math.PI*2);
          ctx.fillStyle = '#0095DD';
          ctx.fill();
          ctx.closePath();
        }
        
        function drawPaddle() {
          ctx.beginPath();
          ctx.rect(paddle.x, canvas.height - paddle.height, paddle.width, paddle.height);
          ctx.fillStyle = '#0095DD';
          ctx.fill();
          ctx.closePath();
        }
        
        function collisionDetection() {
          for(let c = 0; c < bricks.length; c++) {
            for(let r = 0; r < bricks[c].length; r++) {
              let b = bricks[c][r];
              if(b.status == 1) {
                if(ball.x > b.x && ball.x < b.x + 75 && ball.y > b.y && ball.y < b.y + 20) {
                  ball.dy = -ball.dy;
                  b.status = 0;
                  gameState.score++;
                  if(gameState.score == 5 * 3) {
                    clearInterval(intervalID);
                    let endTime = new Date().getTime();
                    let timeTaken = (endTime - startTime) / 1000;
                    alert('YOU WIN, CONGRATULATIONS! Time taken: ' + timeTaken + ' seconds');
                    gameState.score = 0;
                    Shiny.setInputValue('levelComplete', timeTaken);
                  }
                }
              }
            }
          }
        }
        
        Shiny.addCustomMessageHandler('initialize', function(message) {
          initializeGame(message.speed);
        });
        
        Shiny.addCustomMessageHandler('nextLevel', function(message) {
          ball.dx *= 1.1;
          ball.dy *= 1.1;
          for(let c = 0; c < bricks.length; c++) {
            for(let r = 0; r < bricks[c].length; r++) {
              bricks[c][r].status = 1;
            }
          }
          startTime = new Date().getTime();
        });
      ")
    )
  ),
  fluidRow(
    column(8, offset = 2,
           tags$canvas(id = "gameCanvas", width = "480", height = "320")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize game variables
  gameState <- reactiveValues(ballSpeed = 3, level = 1)
  
  observe({
    session$sendCustomMessage(type = "initialize", message = list(speed = gameState$ballSpeed))
  })
  
  observe({
    invalidateLater(1000 / gameState$ballSpeed, session)
    # Game update logic will go here
  })
  
  observeEvent(input$levelComplete, {
    gameState$level <- gameState$level + 1
    gameState$ballSpeed <- gameState$ballSpeed * 1.1
    session$sendCustomMessage(type = "nextLevel", message = list(level = gameState$level, speed = gameState$ballSpeed))
  })
}

shinyApp(ui = ui, server = server)
