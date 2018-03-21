using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class pointsAdder : MonoBehaviour {

    public int pointsToAdd;

    private ScoreManager theScoreManager;

    private AudioSource coinSound;

	// Use this for initialization
	void Start ()
    {
        theScoreManager = FindObjectOfType<ScoreManager>();
        coinSound = GameObject.Find("CoinSound").GetComponent<AudioSource>();
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    void OnTriggerEnter2D(Collider2D other)
    {
        if (other.gameObject.tag == "Player")
        {
            theScoreManager.AddScore(pointsToAdd);
            gameObject.SetActive(false);

            if (coinSound.isPlaying)
            {
                coinSound.Stop();
                coinSound.Play();
            }
            else
            {
                coinSound.Play();
            }
        }
    }
}
