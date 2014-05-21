var paper = view._scope;
var scorebox = $('#score i');
var levelbox = $('#level i');

var tangible = new Layer();
var visible = new Layer();
project.activeLayer = tangible;

var T = 1000;
Math.seed = 5;
var speed = 10;
var baseSpeedCoefficient = 5;
var bossSpeedCoefficient = 100;
var protagSpeed = 10;
var bulletSpeed = 15;
var start = false;

function Entity(o) {
  var self = this;
  self.alive = true;
  self.visible = false;
  self.initialWeight = self.weight;
}
Entity.prototype.updateWeight = function(weight, flag) {
  var self = this;
  if (flag) {
    self.weight += weight;
  }
  else {
    var c = self.shape.fillColor.clone();
    var step = (weight / self.initialWeight) / 2;
    c.red = c.red - step;
    c.blue = c.blue - step;
    c.green = c.green - step;
    self.shape.fillColor = c;
    self.shape.strokeColor = c;
    self.weight -= weight;
  }
  if (self.weight <= 0) {
    self.kill();
    return false;
  }
  return true;
};
Entity.prototype.getWeight = function () {
  var self = this;
  return self.weight;
};
Entity.prototype.kill = function() {
  var self = this;
  self.shape.remove();
  self.alive = false;
};
Entity.prototype.contains = function(e) {
  var self = this;
  if (self.shape.getBounds().contains(e) && self.alive)
    return true;
  return false;
};
Entity.prototype.getPosition = function() {
  var self = this;
  return self.shape.position;
};
Entity.prototype.getAngle = function() {
  var self = this;
  return self.shape.angle;
};
Entity.prototype.fire = function() {
  var self = this;
  if (self.alive) {
    var s = self.tt.fire(self.shape, self.angle);
    self.bullets = self.bullets.concat(s);
  }
};
Entity.prototype.getInitialPosition = function() {
  var self = this;
  return self.initialPosition;
};
Entity.prototype.setInitialPosition = function(p) {
  var self = this;
  self.initialPosition = p;
};
Entity.prototype.setPosition = function(position) {
  var self = this;
  self.shape.setPosition(position);
};
Entity.prototype.setSpeed = function(speed) {
  var self = this;
  self.speed = view.size.height / (speed * 10);
};
Entity.prototype.getBounds = function() {
  var self = this;
  return self.shape.bounds;
};
Entity.prototype.changeColor = function(c) {
  var self = this;
  self.shape.fillColor = c;
  self.shape.strokeColor = c;
};
Entity.prototype.isVisible = function() {
  var self = this;
  return self.visible;
};
Entity.prototype.makeVisible = function() {
  var self = this;
  self.shape.visible = true;
  self.visible = true;
  tangible.removeChildren([self.shape]);
  visible.addChild(self.shape);
};
Entity.prototype.hide = function() {
  var self = this;
  self.shape.visible = false;
  self.visible = false;
};
Entity.prototype.isAlive = function() {
  var self = this;
  return self.alive;
};

function Upgrade() {
  Entity.call(this);
  var self = this;
}
Upgrade.prototype = new Entity();
Upgrade.prototype.constructor = Upgrade;
Upgrade.prototype.move = function() {
  var self = this;
  if (self.visible)
    self.p.move(self.shape, self.speed);
};
Upgrade.prototype.getName = function() {
  var self = this;
  return self.name;
};
Upgrade.prototype.realPosition = function() {
  var self = this;
  var pos = self.initialPosition;
  var r = pos.col;
  var c = pos.row;
  var p = Environment.actualPosition({
    col: r,
    row: c
  });
  self.shape.setPosition(p);
};

function ShotUpgrade(o) {
  Upgrade.call(this);
  var self = this;
  self.name = o.name;
  self.shot = o.shot;
  self.shape = o.shape;
  self.pattern = o.pattern;
  if (self.pattern)
    self.p = new self.pattern();
  self.equipColor = o.equipColor;
  self.turret = o.turret;
  self.initialPosition = o.initialPosition;
  self.realPosition();
}
ShotUpgrade.prototype = new Upgrade();
ShotUpgrade.prototype.constructor = ShotUpgrade;


function ShieldUpgrade(o) {
  Upgrade.call(this);
  var self = this;
  self.name = o.name;
  self.shape = o.shape;
  self.pattern = o.pattern;
  if (self.pattern)
    self.p = new self.pattern();
  self.equipColor = o.equipColor;
  self.weight = o.weight;
  self.initialPosition = o.initialPosition;
  self.realPosition();
}
ShieldUpgrade.prototype = new Upgrade();
ShieldUpgrade.prototype.constructor = ShieldUpgrade;

function BombUpgrade(o) {
  Upgrade.call(this);
  var self = this;
  self.name = o.name;
  self.shape = o.shape;
  self.count = o.count;
  self.pattern = o.pattern;
  if (self.pattern)
    self.p = new self.pattern();
  self.initialPosition = o.initialPosition;
  self.realPosition();
}
BombUpgrade.prototype = new Upgrade();
BombUpgrade.prototype.constructor = BombUpgrade;
BombUpgrade.prototype.getBombCount = function() {
  var self = this;
  return self.count;
};

function LifeUpgrade(o) {
  Upgrade.call(this);
  var self = this;
  self.name = o.name;
  self.type = o.type;
  self.shape = o.shape;
  self.count = o.count;
  self.pattern = o.pattern;
  if (self.pattern)
    self.p = new self.pattern();
  self.initialPosition = o.initialPosition;
  self.realPosition();
}
LifeUpgrade.prototype = new Upgrade();
LifeUpgrade.prototype.constructor = LifeUpgrade;
LifeUpgrade.prototype.getType = function() {
  var self = this;
  return self.type;
};
LifeUpgrade.prototype.getLifeCount = function() {
  var self = this;
  return self.count;
};



var Upgrades = {
  init: function(upgrades) {
    var self = this;
    self.upgrades = [];
    for (var i in upgrades) {
      if (upgrades.hasOwnProperty(i)) {
        if (i == "shots") {
          var shots = upgrades[i];
          for (var j = 0; j < shots.length; j++) {
            self.upgrades.push(new ShotUpgrade(shots[j]));
          }
        }
        if (i == "shields") {
          var shields = upgrades[i];
          for (var k = 0; k < shields.length; k++) {
            self.upgrades.push(new ShieldUpgrade(shields[k]));
          }
        }
        if (i == "bombs") {
          var bombs = upgrades[i];
          for (var h = 0; h < bombs.length; h++) {
            self.upgrades.push(new BombUpgrade(bombs[h]));
          }
        }
        if (i == "lives") {
          var lives = upgrades[i];
          for (var l = 0; l < lives.length; l++) {
            self.upgrades.push(new LifeUpgrade(lives[l]));
          }
        }
      }
    }
  },
  move: function() {
    var self = this;
    for (var i = 0; i < self.upgrades.length; i++) {
      self.upgrades[i].move();
    }
  },
  equip: function(shot) {
    var self = this;
    var p = Protagonist.getProtag();
    for (var k = 0; k < self.upgrades.length; k++) {
      var u = self.upgrades[k];
      if (u.contains(p.getPosition()) && u.isVisible()) {
        if (u instanceof ShotUpgrade) {
          p.changeColor(u.equipColor);
          p.updateShot(u.turret, u.shot);
        }
        if (u instanceof ShieldUpgrade) {
          p.changeColor(u.equipColor);
          console.log(p.getWeight());
          p.updateWeight(u.getWeight(),true);
          console.log(p.getWeight());
        }
        if (u instanceof BombUpgrade) {
          p.updateBombCount(u.getBombCount());
        }
        if (u instanceof LifeUpgrade) {
          var t = u.getType();
          var c = u.getLifeCount();
          if (t == "add") {
            p.updateLifeCount(c);
          }
          if (t == "mult")
            p.changeLifeCount(c * p.getRemainingLives());
          Environment.lifeDisplay();
        }
        u.kill();
      }
    }
  },
  makeVisible: function(name) {
    var self = this;
    for (var i in self.upgrades) {
      var u = self.upgrades[i];
      if (u.getName() == name) {
        u.makeVisible();
        // console.log(u);
      }
    }
  },
  hideByName: function(name) {
    var self = this;
    for (var i in self.upgrades) {
      var u = self.upgrades[i];
      if (u.getName() == name) {
        u.hide();
      }
    }
  },
  setSpeed: function(what, speed) {
    var self = this;
    for (var i in self.upgrades) {
      var u = self.upgrades[i];
      if (u.getName() == what) {
        u.setSpeed(speed);
      }
    }
  }
};


function Antag(o) {
  Entity.call(this);
  var self = this;
  self.name = o.name;
  self.weight = o.weight;
  self.shape = o.shape;
  self.tt = o.tt;
  self.bullets = [];
  self.pattern = o.pattern;
  self.lastTimeFired = 0;
  self.fireRate = o.fireRate;
  self.rotation = o.rotation;
  self.group = o.group;
  self.track = o.track;
  self.score = o.score;
  self.rotation = o.rotation;
  if (self.pattern)
    self.p = new self.pattern();
  self.attraction = false;
  self.initialWeight = self.weight;
  self.angle = 0;
  self.boss = false;
}
Antag.prototype = new Entity();
Antag.prototype.constructor = Antag;
Antag.prototype.move = function() {
  var self = this;
  if (self.pattern)
    self.p.move(self.shape, self.speed);
};
Antag.prototype.getName = function() {
  var self = this;
  return self.name;
};
Antag.prototype.getAttraction = function() {
  var self = this;
  return self.attraction;
};
Antag.prototype.getScore = function() {
  var self = this;
  return self.score;
};
Antag.prototype.clone = function(i,g) {
  var self = this;
  return new Antag({
    name: i,
    weight: self.weight,
    shape: self.shape.clone(),
    tt: self.tt,
    bullets: self.bullets,
    pattern: self.pattern,
    track: self.track,
    lastTimeFired: self.lastTimeFired,
    fireRate: self.fireRate,
    rotation: self.rotation,
    score: self.score,
    group: g
  });
};
Antag.prototype.amendName = function(i) {
  var self = this;
  self.name += i;
};
Antag.prototype.setBoss = function() {
  var self = this;
  self.boss = true;
};
Antag.prototype.getRotation = function() {
  var self = this;
  return self.rotation;
};
Antag.prototype.getTrack = function() {
  var self = this;
  return self.track;
};
Antag.prototype.tracking = function() {
  var self = this;
  if (self.visible) {
    var protagPosition = Protagonist.getPosition();
    var md2 = Math.pow(300,2);
    var ax = self.shape.position.x;
    var ay = self.shape.position.y;
    var px = protagPosition.x;
    var py = protagPosition.y;
    var cx = px - ax;
    var cy = py - ay;
    var a = Math.atan2(cy, cx);
    var d2 = (cx*cx) + (cy*cy);
    if (d2 < md2) {
      var f = 5;
      self.attraction = true;
      self.shape.position.x += Math.cos(a) * f;
      self.shape.position.y += Math.sin(a) * f;
    } else {
      self.attraction = false;
    }
  }
};
Antag.prototype.rotate = function() {
  var self = this;
  if (self.rotation.direction === true) {
    self.shape.rotate(self.rotation.speed);
    self.angle += self.rotation.speed;
  }
  else {
    self.shape.rotate(-self.rotation.speed);
    self.angle -= self.rotation.speed;
  }
};
Antag.prototype.isBoss = function() {
  var self = this;
  return self.boss;
};
Antag.prototype.getGroup = function() {
  var self = this;
  return self.group;
};
Antag.prototype.getLastTimeFired = function() {
  var self = this;
  return self.lastTimeFired;
};
Antag.prototype.getFireRate = function() {
  var self = this;
  return self.fireRate;
};
Antag.prototype.setLastTimeFired = function(time) {
  var self = this;
  self.lastTimeFired = time;
};

var Antagonists = {
  init: function(antags) {
    var self = this;
    self.antags = {};
    for (var i in antags) {
      if (antags.hasOwnProperty(i)) {
        var a = new Antag(antags[i]);
        var n = a.getName();
        // a.makeVisible();
        self.antags[n] = a;
      }
    }
  },
  add: function(a) {
    var self = this;
    var n = a.getName();
    var g = a.getGroup();
    self.antags[n] = a;
  },
  tracking: function() {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      var t = a.getTrack();
      if (t) {
        a.tracking();
      }
    }
  },
  getAntags: function() {
    var self = this;
    return self.antags;
  },
  getAntagByName: function(name) {
    var self = this;
    return self.antags[name];
  },
  rotate: function() {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.getRotation())
        a.rotate();
    }
  },
  isAlive: function(name) {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.getName() == name) {
        return a.isAlive();
      }
    }
    return false;
  },
  makeVisibleByGroupName: function(name) {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.getGroup() == name) {
        a.makeVisible();
      }
    }
  },
  killAllVisible: function() {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.isVisible())
        a.kill();
    }
  },
  killAllButBoss: function() {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (!a.isBoss() && a.isVisible())
        a.kill();
    }
  },
  setSpeedByGroupName: function(what, speed) {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.getGroup() == what) {
        a.setSpeed(speed);
      }
    }
  },
  setSpeedByRandomName: function(what, speed) {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.getGroup() == what) {
        a.setSpeed(speed);
        // console.log(a);
      }
    }
  },
  setSpeedByName: function(what, speed) {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.getName() == what) {
        a.setSpeed(speed);
      }
    }
  },
  makeVisibleByName: function(name) {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.getName() == name) {
        a.makeVisible();
      }
    }
  },
  makeBossVisibleByName: function(name) {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.getName() == name) {
        a.makeVisible();
        a.setBoss();
      }
    }
  },
  hideByName: function(name) {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.getGroup() == name) {
        a.hide();
      }
    }
  },
  move: function() {
    var self = this;
    for (var i in self.antags) {
      var a = self.antags[i];
      if (a.isVisible()) {
        var att = a.getAttraction();
        if (!att) {
          a.move();
        }
      }
    }
  },
  fire: function() {
    var self = this;
    for (var i in self.antags) {
      var antag = self.antags[i];
      if (antag.isVisible()) {
        var p = antag.getPosition();
        if ((p.y < view.bounds.height) && (p.x < view.bounds.width)) {
          var lTF = antag.getLastTimeFired();
          var fR = antag.getFireRate();
          var now = Date.now();
          if (now - lTF > fR) {
            antag.setLastTimeFired(now);
            antag.fire();
          }
        }
      }
    }
  },
  bulletMovement: function() {
    var self = this;
    for (var i in self.antags) {
      var antag = self.antags[i];
      for (var j = 0; j < antag.bullets.length; j++) {
        var bullet = antag.bullets[j];
        bullet.move();
        Bullets.negativeCollision(bullet);
      }
    }
  },
  manageBullets: function() {
    var self = this;
    for (var i in self.antags) {
      var antag = self.antags[i];
      if (antag.isVisible()) {
        for (var j = 0; j < antag.bullets.length; j++) {
          var shot = antag.bullets[j];
          shot.destroyIfOutOfView();
        }
      }
    }
  },
};

function MovementPattern() {
  var self = this;
  self.start = 0;
}
MovementPattern.prototype.move = function(shape, speed) {};

function Horizontal() {
  MovementPattern.call(this);
}
Horizontal.prototype = new MovementPattern();
Horizontal.prototype.constructor = Horizontal;
Horizontal.prototype.move = function(shape, speed) {
  var self = this;
  shape.position.x += speed;
};
function Vertical() {
  MovementPattern.call(this);
}
Vertical.prototype = new MovementPattern();
Vertical.prototype.constructor = Vertical;
Vertical.prototype.move = function(shape, speed) {
  var self = this;
  shape.position.y += speed;
};

function PanHorizontal() {
  MovementPattern.call(this);
  var self = this;
  self.direction = 'right';
}
PanHorizontal.prototype = new MovementPattern();
PanHorizontal.prototype.constructor = PanHorizontal;
PanHorizontal.prototype.move = function(shape, speed) {
  var self = this;
  var width = view.bounds.width;
  var barrier = 100;
  var leeway = width - barrier;
  if (self.direction == 'right') {
    shape.position.x += speed;
    if (shape.position.x >= leeway) {
      self.direction = 'left';
    }
  }
  if (self.direction == 'left') {
    shape.position.x -= speed;
    if (shape.position.x <= barrier) {
      self.direction = "right";
    }
  }
};

function PanVertical() {
  MovementPattern.call(this);
  var self = this;
  self.direction = 'down';
}
PanVertical.prototype = new MovementPattern();
PanVertical.prototype.constructor = PanVertical;
PanVertical.prototype.move = function(shape, speed) {
  var self = this;
  var width = view.bounds.height;
  var barrier = 100;
  var leeway = width - barrier;
  if (self.direction == 'down') {
    shape.position.y += speed;
    if (shape.position.y >= leeway) {
      self.direction = 'up';
    }
  }
  if (self.direction == 'up') {
    shape.position.y -= speed;
    if (shape.position.y <= barrier) {
      self.direction = "down";
    }
  }
};

function Step() {
  MovementPattern.call(this);
  var self = this;
  self.direction = 'right';
  self.step = 100;
}
Step.prototype = new MovementPattern();
Step.prototype.constructor = Step;
Step.prototype.move = function(shape, speed) {
  var self = this;
  var width = view.bounds.width;
  var barrier = 100;
  var leeway = width - barrier;
  if (self.direction == 'right') {
    shape.position.x += speed;
    if (shape.position.x >= leeway) {
      self.y = shape.position.y;
      self.last = "right";
      self.direction = 'down';
    }
  }
  if (self.direction == 'down') {
    shape.position.y += speed;
    if (shape.position.y > (self.y + self.step)) {
      if (self.last == "right")
        self.direction = "left";
      if (self.last == "left")
        self.direction = "right";
    }
  }
  if (self.direction == 'left') {
    shape.position.x -= speed;
    if (shape.position.x <= barrier) {
      shape.position.x = barrier;
      self.y = shape.position.y;
      self.last = "left";
      self.direction = "down";
    }
  }
};

function Zigzag() {
  MovementPattern.call(this);
  var self = this;
}
Zigzag.prototype = new MovementPattern();
Zigzag.prototype.constructor = Zigzag;
Zigzag.prototype.move = function(shape, speed) {
  var self = this;
  shape.position.x += 10 * Math.sin( self.start );
  shape.position.y += speed/4;
  self.start += 0.05;
};

function Spiral() {
  MovementPattern.call(this);
}
Spiral.prototype = new MovementPattern();
Spiral.prototype.constructor = Spiral;
Spiral.prototype.move = function(shape, speed) {
  var self = this;
  shape.position.x += 15 * Math.sin( self.start );
  shape.position.y += 15 * Math.cos(self.start) + 1;
  self.start += 0.15;
};

function Wave() {
  MovementPattern.call(this);
  var self = this;
  self.direction = 'down';
}
Wave.prototype = new MovementPattern();
Wave.prototype.constructor = Wave;
Wave.prototype.move = function(shape, speed) {
  var self = this;
  var minY = 100;
  var maxY = 200;
  if (self.direction == 'down') {
    if (shape.position.x <= maxY)
      shape.position.x += speed;
    else
      self.direction = 'up';
  }
  else if (self.direction == 'up')
  {
    if (shape.position.x >= minY)
      shape.position.x -= speed;
    else
      self.direction = 'down';
  }
  shape.position.y += speed;
};

function Circular() {
  MovementPattern.call(this);
  var self = this;
  self.start = 0;
}
Circular.prototype = new MovementPattern();
Circular.prototype.constructor = Circular;
Circular.prototype.move = function(shape, speed) {
  var self = this;
  var f = 10;
  shape.position.x += f * Math.sin( self.start );
  shape.position.y += speed + f * Math.cos(self.start);
  self.start += 0.1;
};

function LShaped(view, limit) {
  MovementPattern.call(this);
  var self = this;
  self.view = view;
  self.limit = self.view.width - limit;
}
LShaped.prototype = new MovementPattern();
LShaped.prototype.constructor = LShaped;
LShaped.prototype.move = function(shape, speed) {
  var self = this;
  shape.position.x += speed;
  if (shape.position.x >= self.limit) {
    shape.position.x -= speed;
    shape.position.y += speed;
  }
};

function Protag(weight, color, tt, bombs, lives) {
  Entity.call(this);
  var self = this;
  self.initialWeight = weight;
  self.weight = weight;
  self.tt = tt;
  self.shots = [];
  self.bombs = bombs;
  self.lives = lives;

  var l = 10;
  self.shape = new Path({
    sideLength: l,
    strokeColor: color,
    fillColor: color,
    strokeWidth: '1',
    selected: false,
    closed: true
  });
  self.shape.segments = [
    [l, l],
    [l * 2, l],
    [l * 2, l * 2],
    [l * 3, 0]
  ];
  self.shape.position = view.bounds.center;
  self.shape.rotate(-45);
  self.shapeBackup = self.shape.clone();
  self.shapeBackup.visible = false;
}
Protag.prototype = new Entity();
Protag.prototype.constructor = Protag;
Protag.prototype.setWeight = function (w) {
  var self = this;
  self.weight = w;
};
Protag.prototype.getShape = function() {
  var self = this;
  return self.shapeBackup;
};
Protag.prototype.moveLeft = function () {
  var self = this;
  self.shape.position.x -= protagSpeed;
};
Protag.prototype.moveRight = function () {
  var self = this;
  self.shape.position.x += protagSpeed;
};
Protag.prototype.moveUp = function () {
  var self = this;
  self.shape.position.y -= protagSpeed;
};
Protag.prototype.moveDown = function () {
  var self = this;
  self.shape.position.y += protagSpeed;
};
Protag.prototype.fire = function () {
  var self = this;
  if (self.alive) {
    var s = self.tt.fire(self.shape);
    self.shots = self.shots.concat(s);
  }
};
Protag.prototype.updateShot = function (turret, shot) {
  var self = this;
  if (turret === 0)
    self.tt.updateMain(shot);
  if (turret === 1)
    self.tt.updateLeft(shot);
  if (turret === 2)
    self.tt.updateRight(shot);
};
Protag.prototype.updateBombCount = function (c) {
  var self = this;
  console.log("protag had:", self.bombs);
  self.bombs += c;
  console.log("protag now has:", self.bombs);
};
Protag.prototype.getBombCount = function () {
  var self = this;
  return self.bombs;
};
Protag.prototype.getRemainingLives = function () {
  var self = this;
  return self.lives;
};
Protag.prototype.updateLifeCount = function (c) {
  var self = this;
  console.log("protag had:", self.lives, "lives");
  self.lives += c;
  console.log("protag now has:", self.lives, "lives");
};
Protag.prototype.reduceLife = function () {
  var self = this;
  console.log("protag had:", self.lives, "lives");
  self.lives -= 1;
  console.log("protag now has:", self.lives, "lives");
};
Protag.prototype.changeLifeCount = function (c) {
  var self = this;
  console.log("protag had:", self.lives, "lives");
  self.lives = c;
  console.log("protag now has:", self.lives, "lives");
};
Protag.prototype.reduceBombCount = function () {
  var self = this;
  console.log("protag had:", self.bombs, "bombs");
  self.bombs -= 1;
  if (self.bombs < 0)
    self.bombs = 0;
  console.log("protag now has:", self.bombs, "bombs");
};
Protag.prototype.resurrect = function() {
  var self = this;

  setTimeout(function() {
    self.alive = true;
    self.shape.position = Environment.getProtagLastPosition();
    self.weight = self.initialWeight;
    self.shape = self.shapeBackup.clone(true);
    self.shape.visible = true;
    visible.addChild(self.shape);
  }, 3000);
};

var Protagonist = {
  init: function(o) {
    var self = this;
    self.protag = new Protag(o.weight, o.color, o.tt, o.bombs, o.lives);
  },
  moveLeft: function() {
    var self = this;
    var pos = self.protag.getPosition();
    var bounds = self.protag.getBounds();
    if (pos.x > bounds.width)
      self.protag.moveLeft();
  },
  moveRight: function() {
    var self = this;
    var pos = self.protag.getPosition();
    var bounds = self.protag.getBounds();
    var limit = Environment.getViewport().width - bounds.width;
    if (pos.x < limit)
      self.protag.moveRight();
  },
  moveUp: function() {
    var self = this;
    var pos = self.protag.getPosition();
    var bounds = self.protag.getBounds();
    if (pos.y > bounds.height)
      self.protag.moveUp();
  },
  moveDown: function() {
    var self = this;
    var pos = self.protag.getPosition();
    var bounds = self.protag.getBounds();
    var limit = Environment.getViewport().height - bounds.height;
    if (pos.y < limit)
      self.protag.moveDown();
  },
  getPosition: function() {
    var self = this;
    return self.protag.getPosition();
  },
  getProtag: function() {
    var self = this;
    return self.protag;
  },
  getShape: function() {
    var self = this;
    return self.protag.getShape();
  },
  changeWeight: function(w) {
    var self = this;
    self.protag.changeWeight(w);
  },
  changeColor: function(c) {
    var self = this;
    self.protag.changeColor(c);
  },
  fire: function() {
    var self = this;
    self.protag.fire();
  },
  getLives: function() {
    var self = this;
    return self.protag.getRemainingLives();
  },
  bulletMovement: function() {
    var self = this;
    for (var i = 0; i < self.protag.shots.length; i++) {
      var shot = self.protag.shots[i];
      shot.move();
      Bullets.positiveCollision(shot);
    }
  },
  clearBullets: function() {
    var self = this;
    for (var i = 0; i < self.protag.shots.length; i++) {
      var shot = self.protag.shots[i];
      var bullets = shot.getBullets();
      for (var j = 0; j < bullets.length; j++) {
        shot.bullets[j].kill();
      }
    }
  },
  kill: function() {
    var self = this;
    self.getProtag().shape.remove();
    self.clearBullets();
  },
  changeShot: function(turret, shot) {
    var self = this;
    self.protag.updateShot(turret, shot);
  },
  bounding: function() {
    var self = this;
    self.protag.bounding();
  },
  resurrect: function() {
    var self = this;
    self.protag.resurrect();
  },
  manageBullets: function() {
    var self = this;
    for (var i = 0; i < self.protag.shots.length; i++) {
      var s = self.protag.shots[i];
      s.destroyIfOutOfView();
    }
  },
  reduceLife: function() {
    var self = this;
    self.protag.reduceLife();
    Environment.lifeDisplay();
  },
  deployBombs: function() {
    var self = this;
    if (self.protag.getBombCount() > 0) {
      Antagonists.killAllButBoss();
      self.protag.reduceBombCount();
    }
  }
};

function Circle(color, radius) {
  return new Path.Circle({
    center: [0,0],
    radius: radius,
    fillColor: color,
    strokeColor: color,
    visible: false
  });
}

function Rectangle(height, width, color) {
  return new Path.Rectangle({
    center: [0, 0],
    width: width,
    height: height,
    fillColor: color,
    strokeColor: color,
    strokeWidth: 4,
    visible: false
  });
}

function Pentagon(radius, color) {
  return new Path.RegularPolygon({
    center: [0, 0],
    sides: 5,
    radius: radius,
    fillColor: color,
    strokeColor: color,
    strokeWidth: 4,
    visible: false
  });
}

function Square(side, color) {
  return new Path.Rectangle({
    center: [0, 0],
    width: side,
    height: side,
    fillColor: color,
    strokeColor: color,
    strokeWidth: 0,
    visible: false
  });
}

function Triangle(sideLength, color) {
  return new Path.RegularPolygon({
    center: [0, 0],
    sides: 3,
    radius: sideLength,
    fillColor: color,
    strokeColor: color,
    strokeWidth: 1,
    visible: false
  });
}

// var r = new Rectangle(30,400,'#2185c5','#fff');
// var f = new Pentagon(90,'#2185c5','#fff');
// var f = new Triangle(90,'#2185c5','#fff');

function Bullet(shape, weight) {
  Entity.call(this);
  var self = this;
  self.shape = shape;
  self.weight = weight;
  self.inView = true;
}
Bullet.prototype = new Entity();
Bullet.prototype.constructor = Bullet;
Bullet.prototype.getPosition = function() {
  var self = this;
  if (self.alive)
    return self.shape.getPosition();
  return false;
};
Bullet.prototype.updateWeight = function(weight) {
  var self = this;
  self.kill();
};

function ShotType() {
  var self = this;
  self.bullets = [];
}
ShotType.prototype.toggleVisibility = function() {
  var self = this;
  for (var i = 0; i < self.bullets.length; i++) {
    if (self.bullets[i].visible)
      self.bullets[i].shape.visible = false;
    else
      self.bullets[i].shape.visible = true;
  }
};
ShotType.prototype.setPosition = function(pos) {
  var self = this;
  for (var i = 0; i < self.bullets.length; i++) {
    self.bullets[i].shape.position.x = pos.x;
    self.bullets[i].shape.position.y = pos.y - 10;
  }
};
ShotType.prototype.clone = function() {
};
ShotType.prototype.getBullets = function() {
  var self = this;
  return self.bullets;
};
ShotType.prototype.destroyIfOutOfView = function() {
  var self = this;
  for (var i = 0; i < self.bullets.length; i++) {
    var b = self.bullets[i];
    var pos = b.getPosition();
    if ((pos.y <= -10) || (pos.x <= -10) || (pos.y >= view.bounds.height + 10) || (pos.x >= view.bounds.width + 10)) {
      b.kill();
      self.bullets.splice(i,1);
    }
  }
};

StraightShot.prototype = new ShotType();
StraightShot.prototype.constructor = StraightShot;
function StraightShot(bullet, polarity, weight) {
  ShotType.call(this);
  var self = this;
  self.bw = weight;
  self.bullets = [];
  self.bullet = bullet;
  self.polarity = polarity;
  self.bullets[0] = new Bullet(self.bullet.clone(), self.bw);
}
StraightShot.prototype.move = function() {
  var self = this;
  if (self.bullets[0]) {
    if (self.polarity)
      self.bullets[0].shape.position.y -= bulletSpeed;
    else self.bullets[0].shape.position.y += bulletSpeed;
  }
};
StraightShot.prototype.clone = function() {
  var self = this;
  return new StraightShot(self.bullet, self.polarity, self.bw);
};

ManyShot.prototype = new ShotType();
ManyShot.prototype.constructor = ManyShot;
function ManyShot(num, b, polarity, weight) {
  ShotType.call(this);
  var self = this;
  self.count = num;
  self.bw = weight;
  self.bullets = [];
  self.polarity = polarity;
  self.bullet = b;
  for (var i = 0; i < num; i++) {
    self.bullets[i] = new Bullet(self.bullet.clone(), self.bw);
  }
}
ManyShot.prototype.move = function() {
  var self = this;
  var b = self.bullets;
  if (self.polarity) {
    for (var i = 0; i < b.length; i++) {
      if (self.bullets[i]) {
        b[i].shape.position.x -= (i - Math.floor(b.length/2)) * 2;
        b[i].shape.position.y -= bulletSpeed;
      }
    }
  } else {
    for (var j = 0; j < b.length; j++) {
      if (self.bullets[j]) {
        b[j].shape.position.x += (j - Math.floor(b.length/2)) * 2;
        b[j].shape.position.y += bulletSpeed;
      }
    }
  }
};
ManyShot.prototype.clone = function() {
  var self = this;
  return new ManyShot(self.count, self.bullet, self.polarity, self.bw);
};


FibShot.prototype = new ShotType();
FibShot.prototype.constructor = FibShot;
function FibShot(num, polarity) {
  ShotType.call(this);
  var self = this;
  self.count = num;
  self.bullets = [];
  self.polarity = polarity;
  for (var i = 0; i < num; i++) {
    self.bullets[i] = new Bullet(new Circle("green",2), 30);
  }
}
FibShot.prototype.move = function() {
  var self = this;
  var b = self.bullets;
  var height = 250;
  var width = 500;

  var phi = (Math.sqrt(5)+1)/2 - 1;            // golden ratio
  var golden_angle = phi*2*Math.PI;            // golden angle
  var lg_rad = width * 0.45;

  var sm_rad = 2;
  var cx = width;
  var cy = height;

  if (self.polarity) {
    for (var i = 0; i < b.length; i++) {
      var r = i/b.length;
      var a = i*golden_angle;
      var s = ratio * lg_rad;
      b[i].shape.position.x += Math.cos(a) * s;
      b[i].shape.position.y += Math.sin(a) * s;
    }
  } else {
    for (var j = 0; j < b.length; j++) {
      var ratio = j/b.length;
      var angle = j*golden_angle;
      var spiral_rad = ratio * lg_rad;
      b[j].shape.position.x -= Math.cos(angle) * spiral_rad;
      b[j].shape.position.y -= Math.sin(angle) * spiral_rad;
    }
  }
};
FibShot.prototype.clone = function() {
  var self = this;
  return new FibShot(self.count, self.polarity);
};

var Bullets = {
  init: function() {
    var self = this;
    self.bullets = [];
  },
  move: function() {
    Protagonist.bulletMovement();
    Antagonists.bulletMovement();
  },
  manage: function() {
    Protagonist.manageBullets();
    Antagonists.manageBullets();
  },
  positiveCollision: function(shot) {
    var self = this;
    var antags = Antagonists.getAntags();
    var bullets = shot.getBullets();
    for (var j in antags) {
      for (var k = 0; k < bullets.length; k++) {
        var a = antags[j];
        var bp = bullets[k];
        if (a.isVisible() && a.contains(bp.getPosition()) && bp.isAlive()) {
          bp.kill();
          var res = a.updateWeight(bp.getWeight());
          bp.updateWeight(a.getWeight());
          if (res === false) {
            Environment.updateScore(a.getScore());
          }
        }
      }
    }
  },
  negativeCollision: function(shot) {
    var self = this;
    var protag = Protagonist.getProtag();
    var bullets = shot.getBullets();
    for (var k = 0; k < bullets.length; k++) {
      var bp = bullets[k];
      if (protag.contains(bp.getPosition())) {
        var res = protag.updateWeight(bp.getWeight());
        bp.updateWeight(protag.getWeight());
        if (res === false) {
          console.log("PROTAG DEAD");
          Protagonist.reduceLife();
          console.log("RESPAWN IN 3 SECONDS");
          Protagonist.resurrect();
        }
      }
    }
  },
  getBullets: function () {
    var self = this;
    return self.bullets;
  }
};


function TurretType() {
  var self = this;
}

function TraditionalTurret(main, left, right) {
  TurretType.call(this);
  var self = this;
  self.main = main;
  self.left = left;
  self.right = right;
}
TraditionalTurret.prototype = new TurretType();
TraditionalTurret.prototype.constructor = TraditionalTurret;
TraditionalTurret.prototype.fire = function(shape, angle) {
  var self = this;
  var shots = [];
  if (self.main)
    shots.push(self.fireMainTurret(shape, angle));
  if (self.left)
    shots.push(self.fireLeftTurret(shape, angle));
  if (self.right)
    shots.push(self.fireRightTurret(shape, angle));
  return shots;
};
TraditionalTurret.prototype.fireMainTurret = function(shape, angle) {
  var self = this;
  var s = self.main.clone();
  var bounds = shape.getBounds();
  var pos = {
    x: bounds.center.x,
    y: bounds.center.y + 10
  };
  s.toggleVisibility();
  s.setPosition(pos);
  return s;
};
TraditionalTurret.prototype.fireLeftTurret = function(shape, angle) {
  var self = this;
  var s = self.left.clone();
  var bounds = shape.getBounds();
  var pos = {
    x: bounds.center.x - 15,
    y: bounds.center.y + 20
  };
  s.toggleVisibility();
  s.setPosition(pos);
  return s;
};
TraditionalTurret.prototype.fireRightTurret = function(shape, angle) {
  var self = this;
  var s = self.right.clone();
  var bounds = shape.getBounds();
  var pos = {
    x: bounds.center.x + 15,
    y: bounds.center.y + 20
  };
  s.toggleVisibility();
  s.setPosition(pos);
  return s;
};
TraditionalTurret.prototype.updateMain = function(shot) {
  var self = this;
  console.log("main updated");
  self.main = shot;
};
TraditionalTurret.prototype.updateLeft = function(shot) {
  var self = this;
  console.log("left updated");
  self.left = shot;
};
TraditionalTurret.prototype.updateRight = function(shot) {
  var self = this;
  console.log("right updated");
  self.right = shot;
};

function DoubleTurret(left, right) {
  TurretType.call(this);
  var self = this;
  self.left = left;
  self.right = right;
}
DoubleTurret.prototype = new TurretType();
DoubleTurret.prototype.constructor = DoubleTurret;
DoubleTurret.prototype.fire = function(shape, angle) {
  var self = this;
  var shots = [];
  shots.push(self.fireLeftTurret(shape));
  shots.push(self.fireRightTurret(shape));
  return shots;
};
DoubleTurret.prototype.fireLeftTurret = function(shape, angle) {
  var self = this;
  var s = self.left.clone();
  var bounds = shape.getBounds();
  var pos = {
    x: bounds.center.x - 10,
    y: bounds.center.y + 20
  };
  s.toggleVisibility();
  s.setPosition(pos);
  return s;
};
DoubleTurret.prototype.fireRightTurret = function(shape, angle) {
  var self = this;
  var s = self.right.clone();
  var bounds = shape.getBounds();
  var pos = {
    x: bounds.center.x + 10,
    y: bounds.center.y + 20
  };
  s.toggleVisibility();
  s.setPosition(pos);
  return s;
};

function QuadTurret(rb, lb, rt, lt) {
  TurretType.call(this);
  var self = this;
  self.rb = rb;
  self.lb = lb;
  self.rt = rt;
  self.lt = lt;
}
QuadTurret.prototype = new TurretType();
QuadTurret.prototype.constructor = QuadTurret;
QuadTurret.prototype.fire = function(shape, angle) {
  var self = this;
  var shots = [];
  shots.push(self.fireLeftBottom(shape));
  shots.push(self.fireRightBottom(shape));
  shots.push(self.fireLeftTop(shape));
  shots.push(self.fireRightTop(shape));
  return shots;
};
QuadTurret.prototype.fireLeftBottom = function(shape, angle) {
  var self = this;
  var s = self.lb.clone();
  var bounds = shape.getBounds();
  var pos = {
    x: bounds.center.x - 20,
    y: bounds.center.y + 25
  };
  s.toggleVisibility();
  s.setPosition(pos);
  return s;
};
QuadTurret.prototype.fireRightBottom = function(shape, angle) {
  var self = this;
  var s = self.rb.clone();
  var bounds = shape.getBounds();
  var pos = {
    x: bounds.center.x + 20,
    y: bounds.center.y + 25
  };
  s.toggleVisibility();
  s.setPosition(pos);
  return s;
};
QuadTurret.prototype.fireLeftTop = function(shape, angle) {
  var self = this;
  var s = self.lt.clone();
  var bounds = shape.getBounds();
  var pos = {
    x: bounds.center.x - 20,
    y: bounds.center.y - 25
  };
  s.toggleVisibility();
  s.setPosition(pos);
  return s;
};
QuadTurret.prototype.fireRightTop = function(shape, angle) {
  var self = this;
  var s = self.rt.clone();
  var bounds = shape.getBounds();
  var pos = {
    x: bounds.center.x + 20,
    y: bounds.center.y - 25
  };
  s.toggleVisibility();
  s.setPosition(pos);
  return s;
};

function onFrame(e) {
  if (Environment.begin()) {
    if (!Environment.getPaused()) {
      if (Levels.initialized())
        Levels.handle();

      if (Protagonist.getProtag()) {
        if (Key.isDown('left')) {
          Protagonist.moveLeft();
        }
        if (Key.isDown('right')) {
          Protagonist.moveRight();
        }
        if (Key.isDown('up')) {
          Protagonist.moveUp();
        }
        if (Key.isDown('down')) {
          Protagonist.moveDown();
        }

        Bullets.move();
        Bullets.manage();
        Antagonists.move();
        Antagonists.fire();
        Antagonists.tracking();
        Antagonists.rotate();
        Upgrades.move();
        Upgrades.equip();
        Environment.pauseTimer();
        Randoms.makeVisible();
      }
    }
  }
}

function onKeyUp(e) {
  if (e.key == "p") {
    Environment.togglePause();
  }

  if (!Environment.getPaused()) {
    if (e.key == 'space') {
      if (Protagonist.getProtag())
        Environment.fire();
    }

    if (e.key == "z")
      Protagonist.deployBombs();
  }
}

function Single(o) {
  var self = this;
  self.name = o.name;
  self.using = o.using;
  self.initialPosition = o.initialPosition;
  var a = Antagonists.getAntagByName(self.using);
  var d = a.clone(self.name, self.name);
  var p = Environment.actualPosition({
    col: self.initialPosition.col,
    row: self.initialPosition.row
  });
  d.setPosition(p);
  Antagonists.add(d);
}
Single.prototype.getName = function() {
  var self = this;
  return self.name;
};

function Random(o, grid) {
  var self = this;
  self.name = o.name;
  self.using = o.using;
  self.side = o.side;
  self.count = o.count;
  self.wait = o.wait * T;
  self.antagNames = [];
  for (var i = 0; i < self.count; i++) {
    var a = Antagonists.getAntagByName(self.using);
    var n = self.name + i;
    var d = a.clone(n, self.name);
    var p = self.assignPosition(grid);
    self.antagNames[i] = n;
    d.setPosition(p);
    Antagonists.add(d);
  }
}
Random.prototype.getName = function() {
  var self = this;
  return self.name;
};
Random.prototype.getAntagNames = function() {
  var self = this;
  return self.antagNames;
};
Random.prototype.getWait = function() {
  var self = this;
  return self.wait;
};
Random.prototype.assignPosition = function(grid) {
  var self = this;
  var p = arbitrary(grid);
  var o = {};
  switch (self.side) {
    case "left": o = {
      col: 0,
      row: p
    }; break;
    case "right": o = {
      col: grid,
      row: p
    }; break;
    case "top": o = {
      col: p,
      row: 0
    }; break;
    case "bottom": o = {
      col: p,
      row: grid
    }; break;
  }
  return Environment.actualPosition(o);
};

function arbitrary(max) {
  return Math.round(Math.random() * max,0);
}

function randomizer(r, l) {
  var self = this;
  if (r) {
    self.r = r;
    self.last = Date.now();
    self.current = l;
  }
  if (self.r && self.current == Environment.getLevel()) {
    var n = self.r.getAntagNames();
    var w = self.r.getWait();

    for (var i = 0; i < n.length; i++) {
      var a = Antagonists.getAntagByName(n[i]);
      var elapsed = Date.now() - self.last;
      if (elapsed >= w) {
        Antagonists.makeVisibleByName(n[i]);
        n.splice(i,1);
        self.last = Date.now();
      }
    }
  }
}

var Randoms = {
  init: function(randoms) {
    var self = this;
    var grid = Environment.getGrid();
    self.randoms = {};
    self.last = 0;
    for (var i = 0; i < randoms.length; i++) {
      var r = new Random(randoms[i], grid);
      var n = r.getName();
      self.randoms[n] = r;
    }
  },
  getRandomByName: function(name) {
    var self = this;
    return self.randoms[name];
  },
  makeVisible: function(name, l) {
    var self = this;
    var r = self.randoms[name];
    randomizer(r, l);
  },
  isAlive: function(name) {
    return Antagonists.isAlive(name);
  },
  setSpeed: function(what, duration) {
    var self = this;
    Antagonists.setSpeedByRandomName(what, duration);
  }
};

var Singles = {
  init: function(singles) {
    var self = this;
    self.singles = {};
    for (var i = 0; i < singles.length; i++) {
      var g = new Single(singles[i]);
      var n = g.getName();
      self.singles[n] = g;
    }
  },
  getSingleByName: function(name) {
    var self = this;
    return self.singles[name];
  },
  makeVisible: function(name) {
    Antagonists.makeVisibleByName(name);
  },
  makeBossVisible: function(name) {
    Antagonists.makeBossVisibleByName(name);
  },
  isAlive: function(name) {
    return Antagonists.isAlive(name);
  },
  setSpeed: function(what, duration) {
    var self = this;
    Antagonists.setSpeedByName(what, duration);
  }
};


function Group(o) {
  var self = this;
  self.name = o.name;
  self.using = o.using;
  self.lanes = o.lanes;
  self.count = o.count;
  self.initialPosition = o.initialPosition;
  var a = Antagonists.getAntagByName(o.using);
  var pos = self.initialPosition;
  for (var i = 0; i < o.lanes; i++) {
    for (var j = 0; j < o.count; j++) {
      var d = a.clone(""+self.name+i+j, self.name);
      var r = pos.col + j;
      var c = pos.row + i;
      var p = Environment.actualPosition({
        col: r,
        row: c
      });
      d.setPosition(p);
      Antagonists.add(d);
    }
  }
}
Group.prototype.getName = function() {
  var self = this;
  return self.name;
};

var Groups = {
  init: function(groups) {
    var self = this;
    self.groups = {};
    for (var i = 0; i < groups.length; i++) {
      var g = new Group(groups[i]);
      var n = g.getName();
      self.groups[n] = g;
    }
  },
  getGroupByName: function(name) {
    var self = this;
    return self.groups[name];
  },
  makeVisible: function(name) {
    var self = this;
    Antagonists.makeVisibleByGroupName(name);
  },
  setSpeed: function(what, duration) {
    var self = this;
    Antagonists.setSpeedByGroupName(what, duration);
  }
};

function Level(o) {
  var self = this;
  self.protag = o.protag;
  Protagonist.init(o.protag);
  Environment.setLives(Protagonist.getLives());
  Environment.lifeDisplay();
  self.timeline = [];
  for (var i = 0; i < o.timeline.timestamps.length; i++) {
    self.timeline.push(o.timeline.timestamps[i]);
  }
  self.winCondition = o.winCondition;
}
Level.prototype.getTimeline = function() {
  var self = this;
  return self.timeline;
};
Level.prototype.getWinCondition = function() {
  var self = this;
  return self.winCondition;
};
Level.prototype.kill = function() {
  var self = this;
  Protagonist.kill();
  for (var i = 0; i < self.timeline.length; i++) {
    var t = self.timeline[i];
    if (!t.getPassed())
      t.setPassed();
    Antagonists.killAllVisible();
    visible.removeChildren();
  }
};

function Timestamp(type, begin, what, duration) {
  var self = this;
  self.type = type;
  self.beginAt = begin;
  self.what = what;
  self.duration = duration;
  self.passed = false;
}
Timestamp.prototype.getBeginAt = function() {
  var self = this;
  return self.beginAt;
};
Timestamp.prototype.getDuration = function() {
  var self = this;
  return self.duration;
};
Timestamp.prototype.getWhat = function() {
  var self = this;
  return self.what;
};
Timestamp.prototype.getPassed = function() {
  var self = this;
  return self.passed;
};
Timestamp.prototype.setPassed = function() {
  var self = this;
  self.passed = true;
};
Timestamp.prototype.getType = function() {
  var self = this;
  return self.type;
};

var Levels = {
  init: function(levels) {
    var self = this;
    self.levels = levels;
    self.last = Date.now();
    self.current = 0;
    self.l = new Level(self.levels[self.current]);
  },
  initialized: function() {
    var self = this;
    if (self.levels)
      return true;
    return false;
  },
  handle: function() {
    var self = this;
    var next;
    Environment.updateLevelBox(self.current);
    var timeline = self.l.getTimeline();
    var win = self.l.getWinCondition();
    for (var j = 0; j < timeline.length; j++) {
      self.beganAt = self.last;
      var timestamp = timeline[j];
      var type = timestamp.getType();
      var begin = timestamp.getBeginAt() * T;
      var what = timestamp.getWhat();
      var duration = timestamp.getDuration();
      var a = timestamp.getPassed();
      var elapsed = Date.now() - (self.last + Environment.getPausedAt());

      if (elapsed > begin && !a) {
        if (type === 0) {
          if (duration) {
            Groups.setSpeed(what, duration);
          } else {
            Groups.setSpeed(what, baseSpeedCoefficient);
          }
          Groups.makeVisible(what);
        }
        if (type === 1) {
          if (duration) {
            Upgrades.setSpeed(what, duration);
          } else {
            Upgrades.setSpeed(what, baseSpeedCoefficient);
          }
          Upgrades.makeVisible(what);
        }
        if (type === 2) {
          if (duration) {
            Singles.setSpeed(what, duration);
          } else {
            Singles.setSpeed(what, baseSpeedCoefficient);
          }
          Singles.makeVisible(what);
        }
        if (type === 3) {
          if (duration) {
            Randoms.setSpeed(what, duration);
          } else {
            Randoms.setSpeed(what, duration);
          }
          Randoms.makeVisible(what, self.current);
        }
        timestamp.setPassed();
      }
    }

    if (win.time) {
      var d = (Date.now() - Environment.getPausedAt()) - self.beganAt;
      if (d >= win.time) {
        next = self.current + 1;
        if (self.levels[next]) {
          self.iterateLevel(next);
        } else {
          Environment.gameOver();
        }
      }
    }

    if (win.score) {
      if (Environment.getScore() >= win.score) {
        next = self.current + 1;
        if (self.levels[next]) {
          self.iterateLevel(next);
        } else {
          Environment.gameOver();
        }
      }
    }

    if (win.boss) {
      var done = true;
      for (var i = 0; i < timeline.length; i++) {
        done = done && timeline[i].getPassed();
      }
      if (done) {
        Singles.setSpeed(win.boss, bossSpeedCoefficient);
        Singles.makeBossVisible(win.boss);
        if (!Singles.isAlive(win.boss)) {
          next = self.current + 1;
          if (self.levels[next]) {
            self.iterateLevel(next);
          } else {
            Environment.gameOver();
          }
        }
      }
    }
  },
  iterateLevel: function(next) {
    var self = this;
    self.current = next;
    self.l.kill();
    self.l = new Level(self.levels[self.current]);
    self.last = Date.now() - Environment.getPausedAt();
  }
};

var Environment = {
  init: function() {
    var self = this;
    self.view = view.viewSize;
    self.cols = [];
    self.rows = [];
    self.coords = {x: {},y: {}};
    self.start = false;
    self.score = 0;
    self.lifeGroup = new paper.Group();
    self.protagLastPosition = {};
    self.speed = 6;
    self.paused = false;
    self.pauseAt = 0;
  },
  begin: function(e) {
    var self = this;
    if (e)
      self.start = e;
    return self.start;
  },
  togglePause: function() {
    var self = this;
    self.paused = !self.paused;
  },
  getPaused: function() {
    var self = this;
    return self.paused;
  },
  getPausedAt: function(){
    var self = this;
    return self.pauseAt;
  },
  gameOver: function() {
    project.clear();
    scorebox.html('');
    levelbox.html("GAME OVER");
  },
  pauseTimer: function() {
    var self = this;
    if (self.paused) {
      self.pausedAt = Date.now();
    }
  },
  updateScore: function(s) {
    var self = this;
    self.score += s;
    scorebox.html(self.score);
  },
  changeSpeed: function(T) {
    speed = view.size.height / (T * 10);
  },
  getSpeed: function() {
    var self = this;
    return self.speed;
  },
  getScore: function() {
    var self = this;
    return self.score;
  },
  fire: function() {
    var self = this;
    Protagonist.fire();
  },
  equip: function(shot) {
    var self = this;
    self.equipped = shot;
  },
  getViewport: function() {
    var self = this;
    return self.view;
  },
  getGrid: function() {
    var self = this;
    return self.breakpoints;
  },
  grid: function(breakpoints) {
    var self = this,
      wm = Math.round(self.view.width / breakpoints),
      hm = Math.round(self.view.height / breakpoints);
    self.breakpoints = breakpoints;
    for (var i = 0; i <= breakpoints; i++) {
      var x = wm * i,
        y = hm * i;
      self.coords.x[i] = x;
      self.coords.y[i] = y;
    }
  },
  setLives: function(l) {
    var self = this;
    self.lives = l;
  },
  lifeDisplay: function() {
    var self = this;
    var s = Protagonist.getShape();
    var lives = Protagonist.getLives();
    self.lifeGroup.removeChildren();

    if (lives < 10) {
      if (self.text)
        self.text.remove();
      for (var i = 0; i < self.lives-1; i++) {
        var m = s.clone();
        m.rotate(30);
        m.scale(0.75);
        m.visible = true;
        m.position = [30 + (1.25*i) * m.bounds.width, 30];
        self.lifeGroup.addChild(m);
      }
    }
    else {
      var d = s.clone();
      if (self.text)
        self.text.remove();
      d.rotate(30);
      d.scale(0.75);
      d.visible = true;
      d.position = [30,30];
      self.text = new PointText({
          content: 'x ' + lives,
          fillColor: s.fillColor,
          strokeColor: s.strokeColor,
          fontSize: 15
      });
      self.text.position = [d.bounds.width * 4.75, 31];
      self.lifeGroup.addChild(d);
    }
  },
  updateLevelBox: function(i) {
    var self = this;
    levelbox.html("Level " + (i+1));
    self.currentLevel = i;
  },
  getProtagLastPosition: function() {
    var self =  this;
    return self.protagLastPosition;
  },
  actualPosition: function(o) {
    var self = this;
    return {
      x: self.coords.x[o.col],
      y: self.coords.y[o.row]
    };
  },
  getLevel: function(l) {
    var self = this;
    return self.currentLevel;
  }
};

function init() {
}

init();
