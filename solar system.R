# install.packages("shiny")
# install.packages("shinyjs")

library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML("
    #solarSystemCanvas {
      background-color: #000;
      display: block;
      margin: 0 auto;
    }
    .planet-name {
      color: #FFF;
      font-size: 14px;
      position: absolute;
    }
  ")),
  titlePanel("태양계 시뮬레이션"),
  actionButton("startBtn", "시작"),
  tags$canvas(id = "solarSystemCanvas", width = 800, height = 800)
)

server <- function(input, output, session) {
  observeEvent(input$startBtn, {
    runjs("
      const canvas = document.getElementById('solarSystemCanvas');
      const ctx = canvas.getContext('2d');
      const width = canvas.width;
      const height = canvas.height;
      const centerX = width / 2;
      const centerY = height / 2;

      const planets = [
        { name: 'Mercury', radius: 50, size: 4, color: '#aaa', speed: 0.02, angle: 0 },
        { name: 'Venus', radius: 75, size: 6, color: '#ff0', speed: 0.015, angle: 0 },
        { name: 'Earth', radius: 100, size: 6, color: '#00f', speed: 0.01, angle: 0 },
        { name: 'Mars', radius: 125, size: 5, color: '#f00', speed: 0.008, angle: 0 },
        { name: 'Jupiter', radius: 175, size: 10, color: '#ff8c00', speed: 0.005, angle: 0 },
        { name: 'Saturn', radius: 225, size: 9, color: '#ffb700', speed: 0.004, angle: 0 },
        { name: 'Uranus', radius: 275, size: 8, color: '#1e90ff', speed: 0.003, angle: 0 },
        { name: 'Neptune', radius: 325, size: 8, color: '#4169e1', speed: 0.002, angle: 0 }
      ];

      function drawSun() {
        ctx.beginPath();
        ctx.arc(centerX, centerY, 20, 0, 2 * Math.PI);
        ctx.fillStyle = '#ffcc00';
        ctx.fill();
        ctx.closePath();
      }

      function drawOrbit(planet) {
        ctx.beginPath();
        ctx.arc(centerX, centerY, planet.radius, 0, 2 * Math.PI);
        ctx.strokeStyle = '#555';
        ctx.stroke();
        ctx.closePath();
      }

      function drawPlanet(planet) {
        const x = centerX + planet.radius * Math.cos(planet.angle);
        const y = centerY + planet.radius * Math.sin(planet.angle);

        ctx.beginPath();
        ctx.arc(x, y, planet.size, 0, 2 * Math.PI);
        ctx.fillStyle = planet.color;
        ctx.fill();
        ctx.closePath();

        const nameTag = document.createElement('div');
        nameTag.className = 'planet-name';
        nameTag.style.left = `${x + canvas.offsetLeft}px`;
        nameTag.style.top = `${y + canvas.offsetTop}px`;
        nameTag.innerText = planet.name;
        document.body.appendChild(nameTag);
      }

      function updatePlanets() {
        ctx.clearRect(0, 0, width, height);
        drawSun();

        document.querySelectorAll('.planet-name').forEach(el => el.remove());

        planets.forEach(planet => {
          drawOrbit(planet);
          planet.angle += planet.speed;
          drawPlanet(planet);
        });

        requestAnimationFrame(updatePlanets);
      }

      updatePlanets();
    ")
  })
}

shinyApp(ui, server)
