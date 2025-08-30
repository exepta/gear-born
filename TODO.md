1. Ladezustände & Reihenfolge

- Neue Lade-Pipeline: BaseGen → WaterGen → InGame.
- BaseGen: Terrain/Chunks vollständig generieren & in den ChunkMap speichern.
- Wenn alle sichtrelevanten Chunks fertig sind: in WaterGen wechseln.
- WaterGen läuft einmalig über alle geladenen Chunks (plus Rand) und erzeugt die Wasser-Datenstruktur. Danach Wechsel nach InGame (Ladescreen weg).

2. Datenmodell für Wasser (ohne Fluss-Sim)

- Eigene Wasser-Schicht pro Chunk (separat vom Blockgitter), bool/bitset pro Voxel: „hier steht Wasserblock“.
- Konfigurierbarer Meeresspiegel Y_sea (dein Band 20–62: wir füllen „unter Gelände und ≤ Y_sea“; Seen/Teiche auch), vorerst keine Flüsse/Strömung.
- Persistenz: Speichern/Laden der Wasser-Schicht zusammen mit dem Chunk (wie bei Blöcken).

3. Fülllogik (einmalig in WaterGen)

- Für jedes (x,z): höchste feste Blockhöhe ermitteln; alle luftgefüllten Zellen bis Y_sea mit Wasser markieren (Ozean/Küsten).
- Becken/Teiche: Alle „Lufttaschen“ unterhalb Y_sea, die einen geschlossenen Rand haben, bis zum lokalen Pegel auffüllen. (Einfacher Start: alles ≤ Y_sea auffüllen, was nicht von Solid verdrängt wird.)
- Ergebnis: statisches Wasser-Volumen, keine Simulation, nur Belegung.

4. Meshing-Regeln (Wasser als Block 1.5³, keine Innenflächen)

- Beim Wasser-Meshing pro Voxel nur Flächen rendern, die an Luft oder Solid grenzen; keine Flächen, die an Wasser grenzen.
- Top-Fläche nur, wenn die Zelle darüber kein Wasser (und kein Solid) hat.
- Chunk-Rand sauber behandeln: für Nachbarabfrage die Nachbar-Wasserbelegung (Border/Snapshot) mit einbeziehen.
- Material/Transparenz beibehalten; die 1.5-Skalierung visuell anwenden (aber Kanten zwischen Wasserblöcken trotzdem unsichtbar, weil dort keine Flächen emittiert werden).

5. Integration & Laufzeit

- Alte Fluss-Sim & Events entfernen.
- WaterGen triggert Remesh für betroffene Subchunks (Wasser + evtl. angrenzende Terrainmeshes).
- Beim Streamen neuer Chunks: erst Terrain rein, dann lokale WaterGen für diese Chunks (inkl. ein Chunk Rand), dann Wasser-Mesh bauen.
- Unload: Wasser-Schicht mit entladen, beim Re-Load wieder von Disk/Cache.