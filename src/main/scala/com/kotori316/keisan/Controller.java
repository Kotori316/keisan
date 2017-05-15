package com.kotori316.keisan;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.CheckMenuItem;
import javafx.scene.control.Label;
import javafx.scene.control.MenuBar;
import javafx.scene.control.TextField;

public class Controller implements Initializable {

    @FXML
    public Button button0;
    @FXML
    public Button button1;
    @FXML
    public Button button2;
    @FXML
    public Button button3;
    @FXML
    public Button button4;
    @FXML
    public Button button5;
    @FXML
    public Button button6;
    @FXML
    public Button button7;
    @FXML
    public Button button8;
    @FXML
    public Button button9;
    @FXML
    public Button buttonDot;
    @FXML
    public Button buttonPlus;
    @FXML
    public Button buttonMinus;
    @FXML
    public Button buttonMultiply;
    @FXML
    public Button buttonDivide;
    @FXML
    public Button buttonEqual;
    @FXML
    public Button buttonBack;
    @FXML
    public Button buttonClear;
    @FXML
    public Label labelResult;
    @FXML
    public TextField textfield;
    @FXML
    public Button buttonLeft;
    @FXML
    public Button buttonRight;
    @FXML
    public CheckMenuItem menuDecimal;
    @FXML
    public MenuBar menuBar;

    private Fractions result;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        button0.setOnMouseClicked(event -> textfield.appendText("0"));
        button1.setOnMouseClicked(event -> textfield.appendText("1"));
        button2.setOnMouseClicked(event -> textfield.appendText("2"));
        button3.setOnMouseClicked(event -> textfield.appendText("3"));
        button4.setOnMouseClicked(event -> textfield.appendText("4"));
        button5.setOnMouseClicked(event -> textfield.appendText("5"));
        button6.setOnMouseClicked(event -> textfield.appendText("6"));
        button7.setOnMouseClicked(event -> textfield.appendText("7"));
        button8.setOnMouseClicked(event -> textfield.appendText("8"));
        button9.setOnMouseClicked(event -> textfield.appendText("9"));
        buttonLeft.setOnMouseClicked(event -> textfield.appendText("("));
        buttonRight.setOnMouseClicked(event -> textfield.appendText(")"));
        buttonBack.setOnMouseClicked(event -> {
            int length = textfield.getLength();
            if (length != 0) textfield.deleteText(length - 1, length);
        });
        buttonPlus.setOnMouseClicked(event -> textfield.appendText("+"));
        buttonMinus.setOnMouseClicked(event -> textfield.appendText("-"));
        buttonMultiply.setOnMouseClicked(event -> textfield.appendText("x"));
        buttonDivide.setOnMouseClicked(event -> textfield.appendText("/"));
        buttonDot.setOnMouseClicked(event -> textfield.appendText("."));
        buttonClear.setOnMouseClicked(event -> {
            textfield.clear();
            labelResult.setText("");
        });

        buttonEqual.setOnMouseClicked(event -> calc());
        textfield.setOnAction(event -> calc());
    }

    private void calc() {
        try {
            result = Keisan.calculate(textfield.getText());
            labelResult.setText(result.toString(menuDecimal.isSelected()));
        } catch (ArithmeticException | IllegalArgumentException e) {
            labelResult.setText(e.getMessage());
        } catch (Exception e) {
            labelResult.setText("Invalid");
        } finally {
            textfield.clear();
        }
    }

    @FXML
    @SuppressWarnings("unused")
    private void menu_close(ActionEvent event) {
        Platform.exit();
    }

    @FXML
    @SuppressWarnings("unused")
    private void menu_Decimal(ActionEvent event) {
        if (result != null) {
            labelResult.setText(result.toString(menuDecimal.isSelected()));
        }
    }
}
