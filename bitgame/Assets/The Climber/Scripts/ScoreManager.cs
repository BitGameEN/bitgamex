using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;


public class ScoreManager : MonoBehaviour {

    public static ScoreManager instance;

    public Text pointsText;

    [HideInInspector]
    public float pointsCount;

    public Text pointsTextDisp;

    [HideInInspector]
    public float pointsCountDisp;

    public Text bestPoints;

    [HideInInspector]
    public float bestPointsCount;

    public Text bestPointsDisp;

    [HideInInspector]
    public float bestPointsCountDisp;
    public Text blanceText;
    public Text uidText;

    private void Awake()
    {
        if (instance == null)
            instance = this;
    }
    // Use this for initialization
    void Start ()
    {
		if (PlayerPrefs.HasKey("HighScore"))
        {
            pointsText.text = "0";
            pointsTextDisp.text = "0";
            bestPointsCount = PlayerPrefs.GetFloat("HighScore");
            bestPointsCountDisp = PlayerPrefs.GetFloat("HighScore");
        }
	}
	
	// Update is called once per frame
	void Update ()
    {
        if (pointsCount > bestPointsCount && pointsCountDisp > bestPointsCountDisp)
        {
            bestPointsCount = pointsCount;
            bestPointsCountDisp = pointsCountDisp;

            PlayerPrefs.SetFloat("HighScore", bestPointsCount);
            PlayerPrefs.SetFloat("HighScore", bestPointsCountDisp);
        }

        pointsText.text = "Points : " + Mathf.Round(pointsCount);

        //to be displayed after gameover
        pointsTextDisp.text = "Points : " + Mathf.Round(pointsCountDisp);


        bestPoints.text = "Best : " + Mathf.Round(bestPointsCount);

        //to be displayed after gameover
        bestPointsDisp.text = "Best : " + Mathf.Round(bestPointsCountDisp);

        uidText.text = "userid : "+App.Instance.uid;
        blanceText.text = "balance : "+App.Instance.role_balance;
    }

    public void AddScore(int pointsToAdd)
    {
        pointsCount += pointsToAdd;
        pointsCountDisp += pointsToAdd;
    }

}
