# Simulating Galaxies

---

# Enter Newton

```haskell
-- Magnitude of the acceleration felt by b1 due to b2
force :: Body -> Body -> Acceleration
force b1 b2 =
  g * (mass b2) / (distance b1 b2)^2

-- Magnitude of the acceleration felt by b due to bs
forceTotal :: Body -> [Body] -> Acceleration
forceTotal b bs = sum [force b b' | b' <- bs, b' /= b]
```

---

# n^2

```haskell
-- Accelerate Body
accelBody :: Body -> DT -> Acceleration -> Body
accelBody b dt acc = b { vel = (vel b) + acc * dt }

-- Move Body
moveBody :: Body -> DT -> Body
moveBody b dt = b { xPos = xPos + (vel b) * dt }

-- Move Universe
moveUniv :: DT -> Universe -> Universe
moveUniv dt u = 
  u { bodies = [moveBody (accelBody b dt (forceTotal b bs)) dt | b <- bs] }
```

---

# n log n
## Barnes-Hut Simulation

## Quad Tree!

---

![fit](https://camo.githubusercontent.com/89bde0cd5d06bd4142d2ccd81713b5da2e0926f5/68747470733a2f2f75706c6f61642e77696b696d656469612e6f72672f77696b6970656469612f636f6d6d6f6e732f302f30332f4261726e65735f6875745f757365645f6e6f6465732e706e67)

---

![fit](https://upload.wikimedia.org/wikipedia/commons/f/f8/Barnes_hut_tree.png)

---

# The Hardest Part

## Generating realistic initial conditions

**Random configurations** -> Cold death

**Trial and error configurations** -> Heat Death

---

# Virial Theorem
Provides a general equation that relates the average kinectic energy over time of a stable system consisting of N particles, bound by potential forces

### Plummer and Hernquist -> Spherical Galaxies
### Kuzmin -> Disk Galaxies

---

# A Disk Galaxy
## Kuzmin

---

![](./kuzmin_hot_xy.gif)

---

# A Cosmic Dance
## Milky Way <> Andromeda

---

![](./MW_SpiralDance-XY.gif)

---

![](./mw_SpiralDance-yz.gif)

---

# A Galactic Collision!
## Milky Way <-> Andromeda

---

![](./andromeda_mw_xy.gif)

---


