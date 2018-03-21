using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class enemyDamage : MonoBehaviour {

    //this script is added to all the enemies in the game

    Animator enemyAnimator;

    public float pushBackForce; //the force by which the player is pushed back when it touches the enemy
    
    float nextDamage; //the next damage which the enemy has to do on the player

    public GameObject enemyDeathFX;

    public GameObject coin;


    // Use this for initialization
    void Start ()
    {
        enemyAnimator = GetComponent<Animator>();
        nextDamage = 0f;
	}

    // Update is called once per frame
    void Update()
    {

    }


    void OnCollisionEnter2D(Collision2D other)
    {
        if (other.gameObject.tag == "Player")
        {
            gameObject.SetActive(false);

            pushBack(other.transform);

            Instantiate(enemyDeathFX, transform.position, transform.rotation);

            Instantiate(coin, transform.position, transform.rotation);
        }
    }

    void OnTriggerStay2D(Collider2D other)
    {
        if(other.tag == "Player" && nextDamage < Time.time)
            //checks if the other gameobject is player
        {
            PlayerController playercontroller = other.gameObject.GetComponent<PlayerController>();
            playercontroller.MakeDead();
            //ShareScreenShot.instance.TakeScreenshot();
        }
    }



    // for pushing the player back
    void pushBack(Transform pushedObject)
    {
        Vector2 pushDirection = new Vector2(0, (pushedObject.position.y - transform.position.y)).normalized;
        pushDirection *= pushBackForce;
        Rigidbody2D pushRB = pushedObject.gameObject.GetComponent<Rigidbody2D>();
        pushRB.velocity = Vector2.zero;
        pushRB.AddForce(pushDirection, ForceMode2D.Impulse);
    }


}
