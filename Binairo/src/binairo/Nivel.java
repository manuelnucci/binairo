/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package binairo;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;

/**
 *
 * @author manuc
 */
public class Nivel extends javax.swing.JDialog {

    public final int CELDA_SIZE = 25;
    public final int N;
    public final int M;
    private final int CI;
    private LinkedList<Celda> celdasPorVer;
    private LinkedList<Celda> celdasVistas;
    private Image celdaBlanca;
    private Image celdaBlancaLast;
    private Image celdaNegra;
    private Image celdaNegraLast;
    private Image celdaTablero;
    private ScheduledExecutorService executorService;
    public final int ANCHO;
    public final int ALTO;
    public final int INITIAL_X;
    public final int INITIAL_Y;

    public Nivel(java.awt.Frame parent, boolean modal, int N, int M, int CI, LinkedList<Celda> celdas) {
        super(parent, modal);
        this.N = N + 1;
        this.M = M + 1;
        this.CI = CI;
        this.setTitle("Binairo: " + this.N + "x" + this.M + " Hard");
        initComponents();
        try {
            this.celdaBlanca = ImageIO.read(new File("src\\binairo\\images\\celda_blanca.png"));
            this.celdaBlancaLast = ImageIO.read(new File("src\\binairo\\images\\celda_blanca_last.png"));
            this.celdaNegra = ImageIO.read(new File("src\\binairo\\images\\celda_negra.png"));
            this.celdaNegraLast = ImageIO.read(new File("src\\binairo\\images\\celda_negra_last.png"));
            this.celdaTablero = ImageIO.read(new File("src\\binairo\\images\\celda_tablero.png"));
        } catch (IOException ex) {
            Logger.getLogger(Nivel.class.getName()).log(Level.SEVERE, null, ex);
        }
        this.jPanelBoard.setBackground(Color.WHITE);
        this.jPanelControl.setBackground(Color.DARK_GRAY);
        Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
        this.setLocation(dim.width / 2 - this.getSize().width / 2, dim.height / 2 - this.getSize().height / 2);
        inicializarBoton(this.jButtonAvanzar, "src\\binairo\\images\\next.png");
        inicializarBoton(this.jButtonMenuPrincipal, "src\\binairo\\images\\menu.png");
        inicializarBoton(this.jButtonPlay, "src\\binairo\\images\\play.png");
        inicializarBoton(this.jButtonRetroceder, "src\\binairo\\images\\previous.png");
        inicializarBoton(this.jButtonStop, "src\\binairo\\images\\stop.png");
        this.jButtonAvanzar.setAlignmentX(Component.CENTER_ALIGNMENT);
        this.jButtonMenuPrincipal.setAlignmentX(Component.CENTER_ALIGNMENT);
        this.jButtonPlay.setAlignmentX(Component.CENTER_ALIGNMENT);
        this.jButtonRetroceder.setAlignmentX(Component.CENTER_ALIGNMENT);
        this.jButtonStop.setAlignmentX(Component.CENTER_ALIGNMENT);
        this.pack();
        this.ANCHO = this.jPanelBoard.getWidth();
        this.ALTO = this.jPanelBoard.getHeight();
        this.INITIAL_X = this.ANCHO / 2 - (this.M / 2 * CELDA_SIZE) + CELDA_SIZE / 2;
        this.INITIAL_Y = this.ALTO / 2 - (this.N / 2 * CELDA_SIZE) + CELDA_SIZE / 2;
        this.celdasPorVer = celdas;
        this.celdasVistas = new LinkedList<Celda>();
        for (int i = 0; i < this.CI; i++) {
            this.celdasVistas.addLast(this.celdasPorVer.removeFirst());
        }
        repaint();
        this.setVisible(true);
    }

    @Override
    public void paint(Graphics g) {
        super.paint(g);
        int x;
        int y;
        Celda c1, c2;
        y = this.INITIAL_Y;
        for (int i = 0; i < this.N; i++) {
            x = this.INITIAL_X;
            for (int j = 0; j < this.M; j++) {
                g.drawImage(this.celdaTablero, x, y, null);
                x += CELDA_SIZE;
            }
            y += CELDA_SIZE;
        }
        c1 = this.celdasVistas.removeLast();
        Iterator<Celda> it = this.celdasVistas.iterator();
        x = this.INITIAL_X;
        y = this.INITIAL_Y;
        while (it.hasNext()) {
            c2 = it.next();
            x = this.INITIAL_X + CELDA_SIZE * c2.getJ();
            y = this.INITIAL_Y + CELDA_SIZE * c2.getI();
            if (c2.getC().equalsIgnoreCase("o")) {
                g.drawImage(this.celdaBlanca, x + 1, y + 1, null);
            } else {
                g.drawImage(this.celdaNegra, x + 1, y + 1, null);
            }
        }
        x = this.INITIAL_X + CELDA_SIZE * c1.getJ();
        y = this.INITIAL_Y + CELDA_SIZE * c1.getI();
        if (c1.getC().equalsIgnoreCase("o")) {
            g.drawImage(this.celdaBlancaLast, x + 1, y + 1, null);
        } else {
            g.drawImage(this.celdaNegraLast, x + 1, y + 1, null);
        }
        this.celdasVistas.add(c1);
    }

    private void avanzar() {
        // Quitamos la Celda de la cabeza de la lista de una y la ponemos en la cola de la otra
        if (this.celdasPorVer.isEmpty()) {
            this.jButtonAvanzar.setEnabled(false);
        } else {
            this.celdasVistas.addLast(this.celdasPorVer.removeFirst());
            this.jButtonRetroceder.setEnabled(true);
            this.repaint();
        }
    }

    private void retroceder() {
        // Quitamos el último elemento de la lista de celdas vistas y lo agregamos en la cabeza de la otra
        if (this.celdasVistas.size() > this.CI) {
            this.celdasPorVer.addFirst(this.celdasVistas.removeLast());
            this.jButtonAvanzar.setEnabled(true);
            this.repaint();
        } else {
            this.jButtonRetroceder.setEnabled(false);
        }
    }

    private void inicializarBoton(JButton button, String path) {
        button.setIcon(new ImageIcon(((new ImageIcon(path).getImage().getScaledInstance(25, 25, java.awt.Image.SCALE_SMOOTH)))));
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanelBoard = new javax.swing.JPanel();
        jPanelControl = new javax.swing.JPanel();
        jButtonRetroceder = new javax.swing.JButton();
        jButtonAvanzar = new javax.swing.JButton();
        jButtonPlay = new javax.swing.JButton();
        jButtonStop = new javax.swing.JButton();
        jButtonMenuPrincipal = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setMinimumSize(new java.awt.Dimension(800, 600));
        setModal(true);
        setPreferredSize(new java.awt.Dimension(800, 650));
        setResizable(false);

        jPanelBoard.setMinimumSize(new java.awt.Dimension(200, 200));
        jPanelBoard.setName(""); // NOI18N

        javax.swing.GroupLayout jPanelBoardLayout = new javax.swing.GroupLayout(jPanelBoard);
        jPanelBoard.setLayout(jPanelBoardLayout);
        jPanelBoardLayout.setHorizontalGroup(
            jPanelBoardLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 441, Short.MAX_VALUE)
        );
        jPanelBoardLayout.setVerticalGroup(
            jPanelBoardLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 231, Short.MAX_VALUE)
        );

        getContentPane().add(jPanelBoard, java.awt.BorderLayout.CENTER);

        jPanelControl.setMaximumSize(new java.awt.Dimension(174, 28));
        jPanelControl.setLayout(new java.awt.GridBagLayout());

        jButtonRetroceder.setFocusable(false);
        jButtonRetroceder.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonRetroceder.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonRetroceder.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonRetrocederMouseClicked(evt);
            }
        });
        jButtonRetroceder.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonRetrocederActionPerformed(evt);
            }
        });
        jButtonRetroceder.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                jButtonRetrocederKeyPressed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 6, 6, 0);
        jPanelControl.add(jButtonRetroceder, gridBagConstraints);

        jButtonAvanzar.setFocusable(false);
        jButtonAvanzar.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAvanzar.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonAvanzar.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonAvanzarMouseClicked(evt);
            }
        });
        jButtonAvanzar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonAvanzarActionPerformed(evt);
            }
        });
        jButtonAvanzar.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                jButtonAvanzarKeyPressed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 6, 6, 0);
        jPanelControl.add(jButtonAvanzar, gridBagConstraints);

        jButtonPlay.setFocusable(false);
        jButtonPlay.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonPlay.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonPlay.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPlayActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 6, 6, 0);
        jPanelControl.add(jButtonPlay, gridBagConstraints);

        jButtonStop.setFocusable(false);
        jButtonStop.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonStop.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonStop.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonStopActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 6, 6, 0);
        jPanelControl.add(jButtonStop, gridBagConstraints);

        jButtonMenuPrincipal.setFocusable(false);
        jButtonMenuPrincipal.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonMenuPrincipal.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonMenuPrincipal.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonMenuPrincipalActionPerformed(evt);
            }
        });
        jPanelControl.add(jButtonMenuPrincipal, new java.awt.GridBagConstraints());

        getContentPane().add(jPanelControl, java.awt.BorderLayout.SOUTH);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonRetrocederActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonRetrocederActionPerformed

    }//GEN-LAST:event_jButtonRetrocederActionPerformed

    private void jButtonAvanzarActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonAvanzarActionPerformed

    }//GEN-LAST:event_jButtonAvanzarActionPerformed

    private void jButtonMenuPrincipalActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonMenuPrincipalActionPerformed
        this.executorService.shutdownNow();
        this.dispose();
    }//GEN-LAST:event_jButtonMenuPrincipalActionPerformed

    private void jButtonPlayActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonPlayActionPerformed
        this.jButtonAvanzar.setEnabled(false);
        this.jButtonRetroceder.setEnabled(false);
        this.executorService = Executors.newSingleThreadScheduledExecutor();
        this.executorService.scheduleAtFixedRate(new Runnable() {
            @Override
            public void run() {
                avanzar();
            }
        }, 0, 2, TimeUnit.SECONDS);
    }//GEN-LAST:event_jButtonPlayActionPerformed

    private void jButtonStopActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonStopActionPerformed
        this.executorService.shutdownNow();
        this.jButtonAvanzar.setEnabled(true);
        this.jButtonRetroceder.setEnabled(true);
    }//GEN-LAST:event_jButtonStopActionPerformed

    private void jButtonAvanzarKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jButtonAvanzarKeyPressed
        if (evt.getKeyCode() == KeyEvent.VK_RIGHT) {
            this.avanzar();
        }
    }//GEN-LAST:event_jButtonAvanzarKeyPressed

    private void jButtonRetrocederKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jButtonRetrocederKeyPressed
        if (evt.getKeyCode() == KeyEvent.VK_LEFT) {
            this.retroceder();
        }
    }//GEN-LAST:event_jButtonRetrocederKeyPressed

    private void jButtonRetrocederMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonRetrocederMouseClicked
        this.retroceder();
    }//GEN-LAST:event_jButtonRetrocederMouseClicked

    private void jButtonAvanzarMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonAvanzarMouseClicked
        this.avanzar();
    }//GEN-LAST:event_jButtonAvanzarMouseClicked

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAvanzar;
    private javax.swing.JButton jButtonMenuPrincipal;
    private javax.swing.JButton jButtonPlay;
    private javax.swing.JButton jButtonRetroceder;
    private javax.swing.JButton jButtonStop;
    private javax.swing.JPanel jPanelBoard;
    private javax.swing.JPanel jPanelControl;
    // End of variables declaration//GEN-END:variables
}
